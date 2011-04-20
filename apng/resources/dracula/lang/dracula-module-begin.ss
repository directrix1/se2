#lang scheme

(require "check.ss"
         "../teachpacks/testing.ss"
         "../teachpacks/doublecheck.ss"
         (for-syntax "../proof/proof.ss" "../proof/syntax.ss"))

(provide dracula-module-begin)

(define-syntax (dracula-module-begin stx)
  (syntax-case stx ()
    [(_ . forms)
     (with-syntax ([exports (datum->syntax stx `(,#'all-defined-out))])
       (quasisyntax/loc stx
         (#%module-begin
          (define-values []
            #,(annotate-proof
               (make-proof
                (make-part
                 'Dracula
                 (syntax->loc stx)
                 (map syntax->term (syntax->list #'forms))))
               (syntax/loc stx (values))))
          (provide exports)
          (begin-below . forms)
          (generate-report!)
          (check-properties!))))]))
