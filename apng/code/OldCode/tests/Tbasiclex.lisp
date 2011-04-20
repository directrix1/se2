;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.ss" ("cce" "dracula.plt") "modular" "lang")
(require "../interfaces/Ibasiclex.lisp")
(require "../modules/Mbasiclex.lisp")

(module Tbasiclex
  (import Ibasiclex)
  
  (include-book "list-utilities" :dir :teachpacks)
  (include-book "testing" :dir :teachpacks)

  (check-expect
   (split-on-token "abc" (str->chrs "123abc456"))
   (list (str->chrs "123") (str->chrs "ABC") (str->chrs "456")))
)

(link Test
      (Mbasiclex Tbasiclex))

(invoke Test)
