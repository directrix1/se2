(module eprintf mzscheme
  (provide eprintf)
  (define p (current-error-port))
  (define (eprintf fmt-string . rest)
    (apply fprintf p fmt-string rest)))