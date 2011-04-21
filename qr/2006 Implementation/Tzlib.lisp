;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#| Team Steele
   Software Engineering I
   Tzlib

   Functions to work with a zlib datastream.
|#
(in-package "ACL2")

(require "Izlib.lisp")
(require "Mzlib.lisp")
;(require "MpngUtils.lisp")

(module Tzlib
  (import Izlib)

  (include-book "testing" :dir :teachpacks)
  (include-book "doublecheck" :dir :teachpacks)
    
)

(link Test
      (MpngUtils Mzlib))

(invoke Test)