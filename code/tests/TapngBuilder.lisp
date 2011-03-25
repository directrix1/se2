;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#| Team Steele
   Software Engineering I
   TapngBuilder

   Tests for building APNG files from PNG IHDR and IDAT chunks
|#
(in-package "ACL2")

(require "../interfaces/IapngBuilder.lisp")
(require "../interfaces/IpngUtils.lisp")
(require "../modules/MapngBuilder.lisp")
(require "../modules/MpngUtils.lisp")

(module TapngBuilder
  (import IapngBuilder)

  (include-book "testing" :dir :teachpacks)
  (include-book "doublecheck" :dir :teachpacks)
  
)

(link Test
      (MpngUtils MapngBuilder))

(invoke Test)