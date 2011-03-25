;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#| Team Steele
   Software Engineering II
   TxmlUtils

   Functions to assist with parsing and constructing XML files.
|#
(in-package "ACL2")

(require "../interfaces/IxmlUtils.lisp")
(require "../modules/MxmlUtils.lisp")
(require "../interfaces/IminidomSerializer.lisp")
(require "../modules/MminidomSerializer.lisp")
(require "../interfaces/Ibasiclex.lisp")
(require "../modules/Mbasiclex.lisp")
(require "../interfaces/IminidomParser.lisp")
(require "../modules/MminidomParser.lisp")


(module TxmlUtils
  (import IxmlUtils)
  (import IminidomSerializer)
  (import IminidomParser)

  (include-book "testing" :dir :teachpacks)
  (include-book "doublecheck" :dir :teachpacks)
  
  (include-book "audio" :dir :teachpacks)
  
  (play-wav "../testfiles/rickroll.wav" t)

  (defconst
    *sample*
    (xml-readnode
    "<?xml version=\"1.0\"?>
     <pnga frames=\"3\" plays=\"2\">
	<image src=\"bob0.png\" length=\"1/12\"></image>
	<image src=\"bob1.png\" length=\"1/14\"></image>
	<image src=\"bob2.png\" length=\"1/14\"></image>
     </pnga>"))
  

  (check-expect
    (getFrames nil)
	nil)

  (check-expect
    (getFrames (xml-getchildren *sample*))
	'(("bob0.png" "1/12") ("bob1.png" "1/14") ("bob2.png" "1/14")))
  
  (check-expect
   (prepFrameData nil) nil)
  
  (check-expect
   (prepFrameData '(("bob0.png" "1/12") ("bob1.png" "1/14") ("bob2.png" "1/14")))
   (list (list "image" (list (list "src" "bob0.png") (list "length" "1/12"))nil) 
    (list"image" (list (list "src" "bob1.png") (list "length" "1/14"))nil)
     (list "image" (list (list "src" "bob2.png") (list "length" "1/14"))nil)))
 
  (check-expect
   (parseXML nil)
	nil)

  (check-expect
   (parseXML *sample*)
	'("2" "3" (("bob0.png" "1/12") ("bob1.png" "1/14") ("bob2.png" "1/14"))))
  
  (check-expect
   (writeXML nil nil nil)
   "Invalid data")
  
  (check-expect
   (writeXML nil "1" '(("bob4.png" "1/10")))
   "Invalid data")
  
  (check-expect
   (writeXML "1" nil '(("bob4.png" "1/10")))
   "Invalid data")
  
  (check-expect
   (writeXML "1" "1" nil)
   "Invalid data")

  (check-expect
   (writeXML "1" "1" '(("bob4.png" "1/10")))
	"<?xml version=\"1.0\"?><pnga frames=\"1\" plays=\"1\"><image src=\"bob4.png\" length=\"1/10\"/></pnga>")
  
  (check-expect
   (writeFrames '((1101010 "1/4") (10101 "1/8") (1010001 "1/16")) nil)
   nil)
  
  (check-expect
   (writeFrames nil "rickroll")
   nil)
  
  (check-expect
   (writeFrames '((1101010 "1/4") (10101 "1/8") (1010001 "1/16")) "rickroll")
   '(("rickroll1.png" "1/4") ("rickroll2.png" "1/8") ("rickroll3.png" "1/16")))

  
  )

(link Test
      (Mbasiclex MminidomSerializer MminidomParser MxmlUtils TxmlUtils))

(invoke Test)

(set-state-ok t)


