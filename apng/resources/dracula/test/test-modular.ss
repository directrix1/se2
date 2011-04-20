#lang scheme/base

(require "../private/planet.ss"
         "../proof/proof.ss"
         "../proof/syntax.ss"
         "../lang/acl2-module-v.ss"
         "program-tests.ss")
(require (schemeunit test))

(provide test-modular)

(define (proof->sexp proof)
  (for/list ([name (proof-parts proof)])
    (part->sexp (proof-part proof name))))

(define (part->sexp part)
  (for/list ([i (in-range (part-length part))])
    (term-sexp (part-nth part i))))

(define (program defns [repl null])
  (make-module-program/mred modular-acl2-module-v defns repl))

(define (test/modular name defns repl result proof)
  (let* ([prog (program defns repl)])
    (test-suite name
      (test-case "proof"
        (check-equal?
         (proof->sexp (get-proof (car (check-expand-success prog))))
         proof))
      (test-case "program"
        (check-equal? (check-eval-success prog) result)))))

(define test-modular
  (test-suite "Modular"
    (test/modular "top-level" '[] '[(integerp (+ 1 2 3))] 't '[])

    (test/modular
     "interface + module + invoke"
     '[(interface IIdentity (sig id (x)) (con id-equal (equal (id x) x)))
       (module MIdentity (defun id (x) x) (export IIdentity))
       (invoke MIdentity)]
     '[(id 1)]
     1
     '[[(defun id (x) x)
        (defthm id-equal (equal (id x) x))]])

    (test/modular
     "link + invoke"
     '[(interface IInc (sig inc (x)) (con inc-+ (equal (inc x) (+ x 1))))
       (interface ISkip (sig skip (x)) (con skip-+ (equal (skip x) (+ x 2))))
       (module MInc (defun inc (x) (+ 1 x)) (export IInc))
       (module MInc-Skip (import IInc) (defun skip (x) (inc (inc x))) (export ISkip))
       (link MSkip (MInc MInc-Skip))
       (invoke MSkip)]
     '[(skip 1)]
     3
     '[[(defun inc (x) (+ 1 x))
        (defthm inc-+ (equal (inc x) (+ x 1)))]
       [(progn (defstub inc (x) t)
               (defaxiom inc-+ (equal (inc x) (+ x 1))))
        (defun skip (x) (inc (inc x)))
        (defthm skip-+ (equal (skip x) (+ x 2)))]])

    (test/modular
     "rename + link + invoke"
     '[(interface IInc (sig inc (x)) (con inc-+ (equal (inc x) (+ x 1))))
       (interface ISkip (sig skip (x)) (con skip-+ (equal (skip x) (+ x 2))))
       (module MInc (defun add1 (x) (+ 1 x)) (export IInc (inc add1)))
       (module MInc-Skip (import IInc (inc hop)) (defun skip (x) (hop (hop x))) (export ISkip))
       (link MSkip (MInc MInc-Skip))
       (invoke MSkip)]
     '[(skip 1)]
     3
     '[[(defun add1 (x) (+ 1 x))
        (defthm inc-+ (equal (add1 x) (+ x 1)))]
       [(progn (defstub hop (x) t) (defaxiom inc-+ (equal (hop x) (+ x 1))))
        (defun skip (x) (hop (hop x)))
        (defthm skip-+ (equal (skip x) (+ x 2)))]])

    (test/modular
     "include + rename"
     '[(interface ONE (sig f (x)))
       (interface TWO (include ONE) (sig g (x)) (con f=g (equal (f x) (g x))))
       (module M (import ONE (f it)) (export TWO (g it)))]
     '['dummy]
     'dummy
     '[[(defstub it (x) t)
        (defthm f=g (equal (it x) (it x)))]])

    (test/modular
     "induction (concrete function)"
     '[(interface I (fun f (x) (if (endp x) 0 (1+ (f (cdr x))))))
       (module A (defun f (x) (if (endp x) 0 (1+ (f (cdr x))))) (export I))
       (module B (import I))
       (invoke A)]
     '[(f (list 1 2 3))]
     3
     '[[(defun f (x) (if (endp x) 0 (1+ (f (cdr x)))))
        (defun f (x) (if (endp x) 0 (1+ (f (cdr x)))))]
       [(skip-proofs (defun f (x) (if (endp x) 0 (1+ (f (cdr x))))))]])

    (test/modular
     "multiple returns in signatures"
     '[(interface I (sig f (a b) (c d)))
       (module A
         (defun f (x y)
           (mv (+ x y) (- x y)))
         (export I))
       (module B
         (import I))
       (invoke A)]
     '[(mv-let (a b) (f 3 4) (list a b))]
     (list 7 -1)
     '[[(defun f (x y) (mv (+ x y) (- x y))) (progn)]
       [(defstub f (a b) (mv c d))]])

    (test/modular
     "include-book / teachpack"
     '[(interface I
         (sig replicate (n x))
         (con replicate/list (true-listp (replicate n x))))
       (module LI
         (include-book "list-utilities" :dir :teachpacks)
         (export I))
       (invoke LI)]
     '[(replicate 5 'x)]
     (list 'x 'x 'x 'x 'x)
     '[[(include-book "list-utilities" :dir :teachpacks)
        (defthm replicate/list (true-listp (replicate n x)))]])

    (test/modular
     "include-book / system"
     '[(interface I (sig id (x)) (con ident (equal (id x) x)))
       (module M
         (include-book "arithmetic-3/extra/top-ext" :dir :system)
         (defun id (x) x)
         (export I))
       (invoke M)]
     '[(id 7)]
     7
     '[[(include-book "arithmetic-3/extra/top-ext" :dir :system)
        (defun id (x) x)
        (defthm ident (equal (id x) x))]])

    ))
