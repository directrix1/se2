;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.ss" ("cce" "dracula.plt") "modular" "lang")
#| Team Steele
   Software Engineering I
   Rbridge
  
   Main Linking and Invoking File
|#

(require "modules/Mbasiclex.lisp")
(require "modules/Mxmlminidom.lisp")
(require "modules/Mpsc.lisp")
(require "modules/Mboard.lisp")
(require "modules/Mio.lisp")
(require "modules/Mrankings.lisp")

(link Rbridge 
      (Mbasiclex Mxmlminidom Mboard Mrankings Mpsc Mio))

(invoke Rbridge)

(set-state-ok t)

(main "testfiles/051115A" state)
