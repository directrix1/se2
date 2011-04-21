;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
(require "IMain.lisp")
(require "IBitmap.lisp")
(require "IIO.lisp")
(require "IEncoder.lisp")
(require "IMatrix.lisp")
(require "IQRCode.lisp")
(require "IpngQR.lisp")

(module MMain
  (import IpngQR)
  (import IIO)
  (import IBitmap)
  (import IMatrix)
  (import IQRCode)
  (import IEncoder)
  
  ;L = ~7% correction
  (defun EC:L () (ErrorCorrectionLevel$3 0 1 "L"))
  ;M = ~15% correction
  (defun EC:M () (ErrorCorrectionLevel$3 1 0 "M"))
  ;Q = ~25% correction
  (defun EC:Q () (ErrorCorrectionLevel$3 2 3 "Q"))
  ;H = ~30% correction
  (defun EC:H () (ErrorCorrectionLevel$3 3 2 "H"))
  
  #| encode
text: the text to encode
ecLevel: the error correction lever to use while encoding
filename: the name of the file to write to
|#
  (defun encode (text ecLevel filename topng)
    (write-binary-file
     filename 
     (let 
         ((matrix (ByteMatrix:getArray
                   (QRCode:getMatrix
                    (Encoder:encode text ecLevel)))))
       (if topng
           (makePNG matrix)
           (create-bitmap matrix))) 
     state))
  
  (export IMain))


