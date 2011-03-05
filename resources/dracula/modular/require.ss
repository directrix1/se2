#lang scheme/base

(require "../private/planet.ss")
(require (for-syntax scheme/base (cce syntax)))

(provide require-macro)

(define-for-syntax (expand-require stx)
  (parameterize ([current-syntax stx])
    (syntax-case stx ()
      [(_ . mods)
       (begin
         (for ([m (syntax->list #'mods)])
           (unless (string? (syntax-e m))
             (syntax-error m "expected a filename (string literal)")))
         (syntax/loc stx
           (begin
             (require . mods)
             (provide (all-from-out . mods)))))])))

(define-syntax require-macro expand-require)
