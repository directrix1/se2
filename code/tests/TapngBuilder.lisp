;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#| Team Steele
   Software Engineering I
   TapngBuilder

   Tests for building APNG files from PNG IHDR and IDAT chunks
|#
(in-package "ACL2")

(require "../interfaces/IapngBuilder.lisp")
(require "../interfaces/IpngUtils.lisp")
(require "../modules/MapngBuilder.lisp")
(require "../modules/MpngUtils.lisp")
(require "../modules/Mbasiclex.lisp")

(module TapngBuilder
  (import IapngBuilder)

  (include-book "testing" :dir :teachpacks)
  (include-book "doublecheck" :dir :teachpacks)
  (include-book "audio" :dir :teachpacks)
  
  (check-expect 
    	(verifyPNGSig (list (list *pngSig* 1/2)))
	nil)

  (check-expect
    	(verifyPNGSig nil)
	"Input file is not a valid PNG.")

  (check-expect
    	(buildFrames nil '0)
	nil)

  (check-expect
    	(buildACTL nil nil)
	(makeChunk "acTL" nil))

  (check-expect
    	(buildFCTL nil nil nil nil nil nil nil nil)
	(makeChunk "fcTL" nil))

  (check-expect
    	(validateIHDR nil nil)
	nil)
  
  (check-expect
    	(preparePNGs (list (list *pngSig* 1/2) (list *pngSig* 1/2)))
	(list (list nil nil) (list nil nil)))

  (check-expect
    	(preparePNG nil nil nil)
	(list nil nil))

)

(link Test
      (Mbasiclex MpngUtils MapngBuilder))

(invoke Test)
