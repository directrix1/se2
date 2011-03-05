#lang scheme/base

(require "../private/planet.ss")

(require (for-syntax scheme/base
                     scheme/list
                     scheme/match
                     planet/util
                     syntax/parse
                     syntax/boundmap
                     scheme/require-transform
                     (cce syntax)
                     (cce values)
                     (cce text)
                     "static-rep.ss"
                     "syntax-meta.ss"
                     "../lang/acl2-module-v.ss"
                     "../proof/proof.ss"
                     "../proof/syntax.ss")

         mzlib/etc
         (cce require-provide)
         "keywords.ss"
         "dynamic-rep.ss"
         "../lang/defun.ss"
         "../lang/check.ss"
         "../lang/theorems.ss")

(provide module-macro)

(define-for-syntax (matcher . ids)
  (let* ([table (make-free-identifier-mapping)])
    (for ([id ids])
      (free-identifier-mapping-put! table id #t))
    (lambda (stx)
      (syntax-case stx ()
        [(name . _)
         (identifier? #'name)
         (free-identifier-mapping-get table #'name (lambda () #f))]
        [_ #f]))))

(define-for-syntax (expand-module-export port id)
  (let* ([source (port/static-source port)]
         [concrete (port/static-concrete port)]
         [internals (port/static-sig-names/internal port)]
         [externals (port/static-sig-names/external port)]
         [arguments (port/static-sig-args port)])
    (with-syntax ([exp/dynamic id]
                  [puts externals]
                  [gets internals])
      (values
       concrete
       (syntax/loc source
         (define-values ()
           (begin
             (for ([sym (in-list 'puts)]
                   [fun (in-list (unchecked-arity (list . gets)))])
               (interface/dynamic-put-function! exp/dynamic sym fun))
             (values))))))))

(define-for-syntax (expand-module-import port id)
  (let* ([source (port/static-source port)]
         [abstract (port/static-abstract port)]
         [internals (port/static-sig-names/internal port)]
         [externals (port/static-sig-names/external port)]
         [arguments (port/static-sig-args port)]
         [axioms (port/static-con-names/internal port)])
    (with-syntax ([imp/dynamic id]
                  [gets externals]
                  [(put ...) internals]
                  [(tmp ...) (generate-temporaries externals)]
                  [(args ...) arguments]
                  [axs axioms])
      (values
       abstract
       (syntax/loc source
         (begin
           (define-values [tmp ...]
             (apply values
               (for/list ([sym (in-list 'gets)])
                 (interface/dynamic-get-function imp/dynamic sym))))
           ;; combining multiple imported functions into one definition
           (mutual-recursion (defun put args (tmp . args)) ...)
           ;; combining multiple imported axioms/theorems into one definition
           (define-theorems "axiom" . axs)))))))

(define-for-syntax (expand-definition stx)
  (syntax-case* stx (include-book :dir :system :teachpacks) text=?
    [(include-book file :dir :teachpacks)
     (string-literal? #'file)
     (let*-values ([(module-path)
                    (datum->syntax stx (make-teachpack-require-syntax
                                        (text->string #'file ".ss")))]
                   [(imports sources) (expand-import module-path)]
                   [(names) (map import-local-id imports)])
       (with-syntax ([spec module-path]
                     [(name ...) names]
                     [(temp ...) (generate-temporaries names)])
         (values stx
                 (syntax/loc stx (rename-below [temp name] ...))
                 (syntax/loc stx (require (rename-in spec [name temp] ...))))))]
    [(include-book file :dir :system)
     (string-literal? #'file)
     (values stx (syntax/loc stx (begin)) (syntax/loc stx (begin)))]
    [(include-book file)
     (string-literal? #'file)
     (syntax-error
      stx
      "modules cannot include local books; convert the book to a module")]
    [(include-book . _)
     (syntax-error
      stx
      "expected a book name with :dir :system or :dir :teachpacks")]
    [_ (values stx stx (syntax/loc stx (begin)))]))

(define-for-syntax (expand-module-exports mod id)
  (map2 (lambda (port) (expand-module-export port id))
        (module/static-exports mod)))

(define-for-syntax (expand-module-imports mod id)
  (map2 (lambda (port) (expand-module-import port id))
        (module/static-imports mod)))

(define-for-syntax (expand-definitions mod)
  (map/values 3 expand-definition (module/static-definitions mod)))

(define-syntax (event! stx)
  (syntax-parse stx
    [(_ e:expr)
     (match (syntax-local-context)
       [(? list?) #'(begin)]
       ['expression
        (raise-syntax-error 'module
          (format
           "expected a logical event (definition), but got an expression: ~e"
           (syntax->datum #'e))
          #'e)]
       [ctx
        (raise-syntax-error 'module
          "internal error: expanded in unexpected context: ~e"
          ctx)])]))

(define-for-syntax (expand-dynamic stx)
  (syntax-case stx ()
    [(_ static-name)
     (let* ([import-name #'imp/dynamic]
            [export-name #'exp/dynamic]
            [mod (syntax->meta #:message "not a module" #'static-name)]
            [source (module/static-source mod)])
       (let*-values ([(axms imps) (expand-module-imports mod import-name)]
                     [(thms exps) (expand-module-exports mod export-name)]
                     [(defs runs reqs) (expand-definitions mod)])
       (with-syntax ([imps imps]
                     [exps exps]
                     [(run ...) runs]
                     [reqs reqs]
                     [dynamic-name (refresh-identifier
                                    (module/static-dynamic mod))])
         (annotate-part
          (make-part (syntax-e #'static-name)
                     (syntax->loc stx)
                     (map syntax->term (append axms defs thms)))
          (syntax/loc source
            (begin
              (begin . reqs)
              (define dynamic-name
                (make-module/dynamic
                 (lambda (imp/dynamic)
                   (let ([exp/dynamic (empty-interface/dynamic)])
                     (begin
                      (begin-below
                       (begin . imps)
                       (begin run (event! run)) ...
                       . exps)
                      (interface/dynamic-join exp/dynamic
                                              imp/dynamic))))))))))))]))

(define-for-syntax (parse-port rev-ports stx)
  (syntax-case stx ()
    [(_ ifc [ext int] ...)
     (let* ([ifc (syntax->meta #:message "not an interface" #'ifc)])
       (make-port/static
        stx
        ifc
        (map cons
             (syntax->list #'(ext ...))
             (syntax->list #'(int ...)))
        (map (lambda (inc)
               (or
                (findf (lambda (port)
                         (interface/static-external=?
                          inc (port/static-interface port)))
                       rev-ports)
                (syntax-error stx
                              "no im/export of ~a (required by ~a)"
                              (syntax-e (interface/static-name inc))
                              (syntax-e (interface/static-name ifc)))))
             (interface/static-includes ifc))))]))

(define-for-syntax (parse-ports prior stx-list)
  (if (null? stx-list)
      null
      (let* ([port (parse-port prior (car stx-list))])
      (cons port (parse-ports (cons port prior) (cdr stx-list))))))

(define-for-syntax (parse-exports imports stx-list)
  (parse-ports imports stx-list))

(define-for-syntax (parse-imports stx-list)
  (parse-ports null stx-list))

(define-for-syntax (expand-static stx)
  (syntax-case stx ()
    [(_ static-name original . body)
     (let* ([forms (syntax->list #'body)]
            [imps (filter (matcher #'import) forms)]
            [exps (filter (matcher #'export) forms)]
            [defs (filter-not (matcher #'import #'export) forms)])
       (with-syntax ([imps imps]
                     [exps exps]
                     [defs defs])
         (syntax/loc #'original
           (define-syntax static-name
             (make-syntax-meta
              (let* ([imports (parse-imports (syntax->list #'imps))]
                     [exports (parse-exports imports (syntax->list #'exps))]
                     [definitions (syntax->list #'defs)])
                (make-module/static #'static-name
                                    #'dynamic-name
                                    #'original
                                    imports
                                    exports
                                    definitions))
              (expand-keyword "cannot be used as an expression"))))))]))

(define-for-syntax (expand-module stx)
  (parameterize ([current-syntax stx])
    (syntax-case stx ()
      [(_ static-name . body)
       (parameterize ([current-syntax stx])
         (with-syntax ([original stx])
           (syntax/loc stx
             (begin
               (define-syntaxes (define-static) expand-static)
               (define-syntaxes (define-dynamic) expand-dynamic)
               (define-static static-name original . body)
               (define-dynamic static-name)))))])))

(define-syntax module-macro expand-module)
