;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.ss" ("cce" "dracula.plt") "modular" "lang")

(require "../modules/Mbasiclex.lisp")
(require "../modules/Mrankings.lisp")
(require "../modules/Mxmlminidom.lisp")

(module Trankings
  (import Ixmlminidom)
  (import Irankings)
  
  

(include-book "testing" :dir :teachpacks)

(check-expect (getmatchpointtotal nil) 0))

(link Test
      (Mbasiclex Mxmlminidom Mrankings Trankings))

(invoke Test)
