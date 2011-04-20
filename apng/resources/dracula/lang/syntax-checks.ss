#|
Some predicates to check lexical conventions.
|#
(module syntax-checks mzscheme
  
  (provide (all-defined))
  
  ;; Constants must be surrounded with asterisks.
  (define (legal-constant-name? x)
    (and (identifier? x)
         (regexp-match (regexp "\\*.*\\*") (symbol->string (syntax-e x)))))
  
  ;; is stx an identifier whose name starts with a colon?
  (define (keyword-syntax? stx)
    (and (identifier? stx)
         (let ([str (symbol->string (syntax-e stx))])
           (eq? (string-ref str 0) #\:))))
  
  )