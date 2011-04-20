#|
Keep track of the current package name.  Useful for the ACL2 book -> module
compiler.  That effort has been suspended, though.
|#
(module in-package mzscheme
  ;(require-for-syntax (file "prefix.ss"))
  (provide in-package)

  (define-syntax (in-package stx)
    (syntax-case stx ()
      [(_ pkg) #'(begin)]
      #;[(_ pkg) (begin (current-package (syntax-e #'pkg)) #'(begin))]))
  
  )