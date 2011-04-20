#lang scheme/base
(require (for-syntax scheme/base)
         scribble/eval
         scribble/manual
         scribble/decode
         "../reference/evaluator.ss")

(provide show eval show/eval)

(define-syntax (show stx)
  (syntax-case stx ()
    [(_ def ...)
     (syntax/loc stx
       (schemeblock def ...))]))

(define-syntax (eval stx)
  (syntax-case stx ()
    [(_ (def ...) (int ...))
     (syntax/loc stx
       (interaction #:eval (modular-evaluator 'def ...) int ...))]))

(define-syntax (show/eval stx)
  (syntax-case stx ()
    [(_ (def ...) (int ...))
     (syntax/loc stx
       (make-splice
        (list
         (show def ...)
         (eval (def ...) (int ...)))))]))
