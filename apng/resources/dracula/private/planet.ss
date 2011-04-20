#lang scheme

(require (planet cce/scheme:7:0/require-provide))

(define-planet-package cce cce/scheme:7:2)
(define-planet-package fasttest cce/fasttest:3:8)
(define-planet-package schemeunit schematics/schemeunit:2:11)

(provide cce fasttest schemeunit)
