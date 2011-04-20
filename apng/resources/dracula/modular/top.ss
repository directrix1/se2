#lang scheme/base

(require "../private/planet.ss"
         "../lang/check.ss"
         "module.ss")

(require (for-syntax scheme/base
                     scheme/list
                     (cce syntax)
                     "static-rep.ss"
                     "syntax-meta.ss"
                     "../proof/proof.ss"
                     "../proof/syntax.ss"))

(provide top-interaction-macro module-begin-macro)

(define-for-syntax (expand-top-interaction stx)
  (parameterize ([current-syntax stx])
    (syntax-case stx ()
      [(_ . body) (syntax/loc stx (#%top-interaction . body))])))

(define-for-syntax (get-module-name stx)
  (syntax-case stx (module-macro)
    [(module-macro name . _) (identifier? #'name) #'name]
    [_ #f]))

(define-for-syntax (expand-module-begin stx)
  (parameterize ([current-syntax stx])
    (syntax-case stx ()
      [(_ . body)
       (with-syntax ([exports (datum->syntax stx `(,#'all-defined-out))]
                     [names (filter-map get-module-name (syntax->list #'body))])
         (syntax/loc stx
           (#%module-begin
            (provide exports)
            (begin-below . body)
            (define-values ()
              (annotate-modules names (values))))))])))

(define-for-syntax (module/static->part id mod)
  (make-part
   (syntax-e id)
   (syntax->loc (module/static-source mod))
   (map syntax->term
        (append
         (map port/static-abstract (module/static-imports mod))
         (module/static-definitions mod)
         (map port/static-concrete (module/static-exports mod))))))

(define-for-syntax (identifier->part id)
  (module/static->part id (syntax->meta #:message "not a module" id)))

(define-syntax (annotate-modules stx)
  (syntax-case stx ()
    [(_ names expr)
     (annotate-proof
      (apply make-proof (map identifier->part (syntax->list #'names)))
      #'expr)]))

(define-syntax top-interaction-macro expand-top-interaction)
(define-syntax module-begin-macro expand-module-begin)
