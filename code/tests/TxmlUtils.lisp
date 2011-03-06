;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#| Team Steele
   Software Engineering I
   TxmlUtils

   Functions to assist with parsing and constructing XML files.
|#
(in-package "ACL2")

(require "../interfaces/Ibasiclex.lisp")
(require "../interfaces/IapngBuilder.lisp")
(require "../interfaces/IapngExploder.lisp")
(require "../interfaces/IxmlUtils.lisp")
(require "../interfaces/IminidomSerializer.lisp")


(module TxmlUtils
  (import IxmlUtils)

  (include-book "testing" :dir :teachpacks)
  (include-book "doublecheck" :dir :teachpacks)

  (defconst
    *sample*
    "<pnga frames=\"3\" plays=\"2\">
	<image src=\"bob0.png\" length=\"1/12\"></image>
	<image src="\bob1.png\" length=\"1/14\"></image>
	<image src="\bob2.png\" length=\"1/14\"></image>
     </pnga>")

  (check-expect
    (getFrames nil)
	'(nil nil))

  (check-expect
    (getFrames sample)
	'('(bob0.png 1/12) '(bob1.png 1/14) '(bob2.png 1/14)))

  (check-expect
   (parseXML nil)
	'(nil nil nil))

  (check-expect
   (parseXML sample)
	'(2 3 '(bob0.png 1/12) '(bob1.png 1/14) '(bob2.png 1/14)))

  (check-expect
   (writeXML 1 1 '(bob4.png 1/10))
	'("<pnga frames=\"1\" plays="\1\"><image src=\"bob4.png\" length=\"1/10\"></image></pnga>"))

  
  )

(link Test
      (Mbasiclex MminidomSerializer MpngUtils MapngExploder TxmlUtils))

(invoke Test)

(set-state-ok t)


