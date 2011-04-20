;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#|
2  - tiny
5  - small
9  - medium
19 - large
49 - huge
|#

; 2:30 - 3:30 updating modules

(require "IMain.lisp")

(require "MIO.lisp")
(require "MBitmap.lisp")
(require "MMatrix.lisp")
(require "MQRCode.lisp")

(module MMain
  
  (import IIO) ; io interface guarantees state will be available
  (import IBitmap)
  (import IMatrix)
  (import IQRCode)
  
  (defconst *args-error-1*
    "image->text expects 1 arguments:\nString -> filename")
  
  (defconst *args-error-2*
    "image->textfile expects 2 arguments:\nString -> file in\nString -> file out")
  
  (defconst *args-error-3*
    "text->imagefile expects 2 arguments:\nString -> file in\nString -> file out")
  
  ;Exported functions
  (defun image->text (filename)
    (if (stringp filename)
        (mv-let
         (bytes msg state)
         (read-bitmap-file filename state)
         (if msg
             msg
             (mv (matrix->text (parse-bitmap bytes)) msg state)))
        *args-error-1*))
  
  (defun image->textfile (filein fileout)
    (if (and (stringp filein) (stringp fileout))
        (mv-let
         (data msg state)
         (image->text (image->text filein))
         (write-text-file fileout data state))
        *args-error-2*))
  
  (defun test (filein fileout)
    (mv-let
     (bytes msg state)
     (read-bitmap-file filein state)
     (if msg msg
        (write-bitmap-file fileout (create-bitmap (parse-bitmap bytes))
        state))))
  
  (defun text->imagefile (filein fileout)
    (if (and (stringp filein) (stringp fileout))
        (mv-let
         (str msg state)
         (read-text-file filein state)
         (if msg
             msg
             (write-bitmap-file fileout (create-bitmap (text->matrix str)) state)))
        *args-error-3*))
  
  (export IMain))

(link Program (import) (export IMain) (MQRCode MMatrix MBitmap MIO MMain))
(invoke Program)