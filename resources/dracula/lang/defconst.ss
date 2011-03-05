#lang scheme

(provide defconst)

(require (for-syntax "syntax-checks.ss")
         "check.ss"
         "acl2-app.ss")

(define-syntax (defconst stx)
  (syntax-case stx ()
    [(_ name expr)
     (begin
       (unless (legal-constant-name? #'name)
         (raise-syntax-error #f
           "Constant names must begin and end with asterisks (*)."
           stx #'name))
       (quasisyntax/loc stx
         (begin (define the-const expr)
                (define-below-marker-for here #,stx)
                (define-syntax name
                  (check-below-transformer #'here
                    (lambda (ref)
                      (unless (identifier? ref)
                        (raise-syntax-error #f
                          "invalid reference to defconst name" ref))
                      #'the-const))))))]))
