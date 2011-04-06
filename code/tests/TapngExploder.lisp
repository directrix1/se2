;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#| Team Steele
   Software Engineering II
   TapngExploder

   Tests for exploding APNG files into constituent frames.
|#
(in-package "ACL2")

(require "../interfaces/IapngExloder.lisp")
(require "../interfaces/IpngUtils.lisp") ; XXX needed?
(require "../modules/MapngExloder.lisp")
(require "../modules/MpngUtils.lisp") ; XXX needed?

(module TapngExloder
  (import IapngExploder)

  (include-book "testing" :dir :teachpacks)
  (include-book "doublecheck" :dir :teachpacks)
  (include-book "audio" :dir :teachpacks))

(link Test
  (MpngUtils MapngExploder))

(invoke Test)
