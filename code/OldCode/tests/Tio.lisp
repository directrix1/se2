;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.ss" ("cce" "dracula.plt") "modular" "lang")

(require "../modules/Mio.lisp")
(require "../modules/Mbasiclex.lisp")
(require "../modules/Mboard.lisp")
(require "../modules/Mpsc.lisp")
(require "../modules/Mrankings.lisp")
(require "../modules/Mxmlminidom.lisp")


(link Test
      (Mbasiclex Mxmlminidom Mboard Mrankings Mpsc Mio))

(invoke Test)

(set-state-ok t)

(main "051115A" state)
