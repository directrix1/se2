;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
(require "MUtilities.lisp")
(require "MMatrix.lisp")
(require "MQRCode.lisp")
(require "MEncoder.lisp")
(require "MBitmap.lisp")
(require "MIO.lisp")
(require "MMain.lisp")

(link Program (import) (export IMain)
      (MUtilities MMatrix MQRCode MEncoder MBitmap MIO MMain))
(invoke Program)

#| RUN THIS FILE!!!
to create a QRCode image run the command
(encode content ecLevel output)

content is a string of what you wish to encode

ecLevel can be one of four options:
  (EC:L) = ~7% correction
  (EC:M) = ~15% correction
  (EC:Q) = ~25% correction
  (EC:H) = ~30% correction

output is a string of the output file name for the image you wish to create

EXAMPLE:
(encode "Software Engineering is Fun!" (EC:H) "output.bmp")
this command will encode the string "Software Engineering is Fun!"
into an image file called "output.bmp" with an ErrorCorrectionLevel of high.
|#