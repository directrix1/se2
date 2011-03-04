;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#| Team Steele
   Software Engineering I
   Tpngutils

   Functions to assist with parsing and constructing PNG files.
|#
(in-package "ACL2")

(require "../interfaces/IpngUtils.lisp")
(require "../modules/MpngUtils.lisp")
(require "../modules/Mbasiclex.lisp")


(module TpngUtils
  (import IpngUtils)

  (include-book "testing" :dir :teachpacks)
  (include-book "doublecheck" :dir :teachpacks)
  (include-book "audio" :dir :teachpacks)
    
)

(link Test
      (Mbasiclex MpngUtils TpngUtils))

(invoke Test)