(module constants mzscheme
  (provide (all-defined))

  (define-syntax (nil stx)
    (if (identifier? stx)
        (syntax/loc stx (quote ()))
        (raise-syntax-error #f "cannot be used as a function" stx)))

  (define-syntax (t stx)
    (if (identifier? stx)
        (syntax/loc stx (quote t))
        (raise-syntax-error #f "cannot be used as a function" stx)))

  )
