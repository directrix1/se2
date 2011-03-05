#lang scheme/base

(require scheme/sandbox
         "../private/planet.ss"
         "../lang/acl2-module-v.ss")

(require (cce sandbox))

(provide the-evaluator evaluator modular-evaluator)

(define (evaluator . definitions)
  (apply make-scribble-evaluator acl2-module-v definitions))

(define (modular-evaluator . definitions)
  (apply make-scribble-evaluator modular-acl2-module-v definitions))

(define the-evaluator (evaluator))
