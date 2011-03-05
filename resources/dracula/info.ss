#lang setup/infotab

;; Required for all packages
(define name "Dracula")

;; setup-plt:
(define compile-omit-paths
  (list "acl2/gui.ss" "acl2/new-gui.ss" "acl2/program-controller.ss"))

;; tools:

(define tools (list (list "tool.ss" "drscheme")))
(define tool-names (list "Dracula"))
(define tool-icons (list "images/acl2-icon.png"))

;; planet:

(define blurb '("Provides the Dracula language level for ACL2 emulation."))

(define release-notes
  '("8.16: " "Bug fix for anomalous Dracula/ACL2 interaction. "))

(define categories '(devtools scientific))

(define homepage "http://www.ccs.neu.edu/home/cce/acl2/")

(define primary-file "lang/dracula.ss")

(define required-core-version "4.2.2")

(define repositories '("4.x"))

(define scribblings
  '[("guide/guide.scrbl" [multi-page] [getting-started -30])
    ("reference/reference.scrbl" [multi-page] [language -30])])
