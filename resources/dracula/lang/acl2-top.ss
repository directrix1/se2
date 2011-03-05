#lang scheme

(require "check.ss"
         (for-syntax "syntax-checks.ss"))

(provide (rename-out [acl2-top #%top]))

(define-syntax (acl2-top stx)
  (syntax-case stx ()
    [(_ . keyword)
     (keyword-syntax? #'keyword)
     (syntax/loc stx (#%datum . keyword))]
    [(_ . top)
     (syntax/loc stx (top/error . top))]))
