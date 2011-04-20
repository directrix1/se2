#lang scheme/base

(require scheme/stxparam

         (for-syntax scheme/base
                     "syntax-meta.ss"))

(provide fun sig con include import export include-book)

(define-syntax-parameter fun
  (expand-keyword "not valid outside interface"))
(define-syntax-parameter sig
  (expand-keyword "not valid outside interface"))
(define-syntax-parameter con
  (expand-keyword "not valid outside interface"))
(define-syntax-parameter include
  (expand-keyword "not valid outside interface"))

(define-syntax-parameter include-book
  (expand-keyword "not valid outside module"))

(define-syntax-parameter import
  (expand-keyword "not valid outside module top-level"))
(define-syntax-parameter export
  (expand-keyword "not valid outside module top-level"))
