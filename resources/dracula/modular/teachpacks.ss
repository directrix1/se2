#lang scheme/base

(require scheme/unit "world-teachpack.ss"
         (for-syntax scheme/base "static-rep.ss" "syntax-meta.ss"))

(provide iposn iimage ievent iworld ibigbang
         posn image event bigbang)

(define-for-syntax keyword-error
  (expand-keyword "cannot be used as an expression"))

(define-syntax (define-interface stx)
  (syntax-case stx ()
    [(_ name static)
     (syntax/loc stx
       (define-syntax name
         (make-syntax-meta static keyword-error)))]))

(define-syntax (define-module stx)
  (syntax-case stx ()
    [(_ name dynamic imps exps)
     (with-syntax ([original stx])
       (syntax/loc stx
         (define-syntax name
           (make-syntax-meta
            (make-module/static
             #'name
             #'dynamic
             #'original
             (for/list ([id (in-list (syntax->list #'imps))]
                        [imp (in-list (list . imps))])
               (make-port/static id imp null null))
             (for/list ([id (in-list (syntax->list #'exps))]
                        [exp (in-list (list . exps))])
               (make-port/static id exp null null))
             null)
            keyword-error))))]))

(define-interface iposn posn-interface)
(define-interface iimage image-interface)
(define-interface ievent event-interface)
(define-interface iworld world-interface)
(define-interface ibigbang bigbang-interface)

(define-module posn posn-module [] [posn-interface])
(define-module image image-module [] [image-interface])
(define-module event event-module [] [event-interface])
(define-module bigbang bigbang-module [world-interface] [bigbang-interface])
