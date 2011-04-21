;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#| Team Steele
   Software Engineering I
   TpngQR

   Functions to construct PNG files from QR matrices.
|#
(in-package "ACL2")

(require "IpngQR.lisp")
(require "MpngQR.lisp")
(require "MpngUtils.lisp")
(require "Mzlib.lisp")
(require "Mbasiclex.lisp")

(module TpngQR
  (import IpngQR)

  (include-book "testing" :dir :teachpacks)
  (include-book "doublecheck" :dir :teachpacks)
    
)

(link Test
      (Mbasiclex MpngUtils Mzlib MpngQR))

(invoke Test)