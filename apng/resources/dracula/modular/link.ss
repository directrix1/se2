#lang scheme/base

(require "../private/planet.ss"
         "dynamic-rep.ss")

(require (for-syntax scheme/base
                     (cce syntax)
                     "static-rep.ss"
                     "syntax-meta.ss"
                     "list-set.ss"))

(provide link-macro)

(define-for-syntax (expand-link stx)
  (parameterize ([current-syntax stx])
    (syntax-case stx ()
      [(_ name ())
       (syntax-error stx "must link at least one module")]
      [(_ name (mod))
       (syntax/loc stx
         (define-syntax name (syntax-local-value #'mod)))]
      [(link name (one two . mods))
       (with-syntax ([original stx])
         (syntax/loc stx
           (begin
             (define-syntaxes (one-two one/impl two/impl)
               (let* ([one/static (syntax->meta #:message "not a module" #'one)]
                      [one/dynamic (module/static-dynamic one/static)]
                      [one/imports (module/static-imports one/static)]
                      [one/exports (module/static-exports one/static)]
                      [two/static (syntax->meta #:message "not a module" #'two)]
                      [two/dynamic (module/static-dynamic two/static)]
                      [two/imports (module/static-imports two/static)]
                      [two/exports (module/static-exports two/static)]
                      [one-two/exports
                       (list-union #:compare port/static-external=?
                                   one/exports two/exports)]
                      [one-two/imports
                       (list-union #:compare port/static-external=?
                                   one/imports
                                   (list-minus #:compare port/static-external=?
                                               two/imports one/exports))])
                 (values
                  (make-syntax-meta
                   (make-module/static #'one-two
                                       #'dynamic
                                       #'original
                                       one-two/imports
                                       one-two/exports
                                       null)
                   (expand-keyword "cannot be used as an expression"))
                  (make-rename-transformer one/dynamic)
                  (make-rename-transformer two/dynamic))))
             (define dynamic
               (make-module/dynamic
                (lambda (imp/dynamic)
                  (let* ([one/func (module/dynamic-implementation one/impl)]
                         [exp-one/dynamic (one/func imp/dynamic)]
                         [imp-two/dynamic
                          (interface/dynamic-join imp/dynamic exp-one/dynamic)]
                         [two/func (module/dynamic-implementation two/impl)]
                         [exp-two/dynamic (two/func imp-two/dynamic)]
                         [exp/dynamic
                          (interface/dynamic-join imp-two/dynamic
                                                  exp-two/dynamic)])
                    exp/dynamic))))
             (link name (one-two . mods)))))]
      [_
       (syntax-error
        stx
        "expected a name followed by a parenthesized list of one or more module names")])))

(define-syntax link-macro expand-link)
