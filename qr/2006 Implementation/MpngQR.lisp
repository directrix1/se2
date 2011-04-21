;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#| Team Steele
   Software Engineering I
   MpngQR

   Functions to construct PNG files from QR matrices.
|#
(in-package "ACL2")

(require "IpngQR.lisp")
(require "IpngUtils.lisp")

(module MpngQR
  ; Returns the matrix scaled by 8 in each direction and multiplied
  ; by 255, where:
  ;  matrix = a list of lists like '((1 0 1 1 ... 0 1)
  ;                                  . . . . . . . . .
  ;                                  (0 1 1 0 ... 1 0))
  (defun scalify (matrix))
  
  ; Returns a row scaled by 8 horizontally and multiplied
  ; by 255, where:
  ;  row = a list like '(1 0 1 1 ... 0 1)
  (defun scalifyRow (row))
  
  ; Adds filter type 0 to the beginning of each row in the matrix where:
  ;  matrix = a list of lists like '((1 0 1 1 ... 0 1)
  ;                                  . . . . . . . . .
  ;                                  (0 1 1 0 ... 1 0))  
  (defun addFilter (matrix))
  
  ; Concatenates a list of byte lists into one long list where:
  ;  bytelists = a list of lists like '((5 0 253 20 ... 0 1)
  ;                                      . . . . . . . . .
  ;                                     (0 1 7 0 ... 9 0))  
  (defun concatByteLists (bytelists))
  
  ; Creates an IDAT chunk when given a matrix where:
  ;  matrix = a list of lists like '((1 0 1 1 ... 0 1)
  ;                                  . . . . . . . . .
  ;                                  (0 1 1 0 ... 1 0))  
  (defun makeIDAT (matrix))
  
  ; Creates a grayscale IHDR chunk with the specified width and height
  ; where:
  ;  width = the width of the PNG image
  ;  height = the height of the PNG image
  (defun makeIHDR (width height))
  
  ; Creates a PNG from the specified matrix where:
  ;  matrix = a list of lists like '((1 0 1 1 ... 0 1)
  ;                                  . . . . . . . . .
  ;                                  (0 1 1 0 ... 1 0))    
  (defun makePNG (matrix))
  
  (export IpngQR))