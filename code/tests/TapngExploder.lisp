;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#| Team Steele
   Software Engineering II
   TapngExploder

   Tests for exploding APNG files into constituent frames.
|#
(in-package "ACL2")

(require "../interfaces/IapngExloder.lisp")
(require "../interfaces/IpngUtils.lisp") ; XXX needed?
(require "../modules/MapngExloder.lisp")
(require "../modules/MpngUtils.lisp") ; XXX needed?

(module TapngExloder
  (import IapngExploder)

  (include-book "testing" :dir :teachpacks)
  (include-book "doublecheck" :dir :teachpacks)
  (include-book "audio" :dir :teachpacks)

  (check-expect 
    	(getFrames nil nil nil nil)
	nil)
  
  (check-expect 
    	(buildDataChunk nil nil nil)
	(list nil nil))

  (check-expect
        (splitByFcTL nil nil)
	(list nil nil))

  (check-expect
    	(splitByFcTL nil (list (list "IDAT" '(42))))
	(list (list (list "IDAT" '(42))) nil))

  (check-expect
    	(splitByFcTL nil (list (list "IDAT" '(42)) (list "IEND" '(65))))
	(list (list (list "IDAT" '(42))) (list (list "IEND" '(65)))))
  
  (check-expect
        (splitByFcTL nil (list (list "IHDR" '(23)) (list "IDAT" '(8))
			       (list "fcTL" '(4))  (list "fdAT" '(15))))
	(list
	  (list (list "IHDR" '(23)) (list "IDAT" '(8)))
	  (list (list "fcTL" '(4))  (list "fdAT" '(15)))))

  (check-expect
      	(makeChunks nil)
	nil)

  (check-expect
    	(splitAtFirstFrameChunk nil nil (list 
				  (list "IHDR" '(23)) (list "IDAT" '(8))
			          (list "fcTL" '(4)) (list "fdAT" '(15))))
	 (list
	  (list (list "IHDR" '(23)) (list "IDAT" '(8)))
	  (list (list "fcTL" '(4))  (list "fdAT" '(15)))))
 
  (check-expect
    	(getIHDR (list (list "IHDR" '(42))))
	'(42))
  
  (check-expect
    	(getIHDR (list (list "fcTL" '(4))))
	nil)

  (check-expect
    	(getIHDR nil)
	nil)
 
  (check-expect
        (getChunksWithName "IHDR" (list (list "IHDR" '(42))
					(list "fcTL" '(4))
					(list "fdAT" '(8))
					(list "IHDR" '(43))))
	(list
	  (list (list "IHDR" '(42)) (list "IHDR" '(43)))
	  (list (list "fcTL" '(4)) (list "fdAT" '(8)))))

  (check-expect
    	(getChunksWithName "IHDR" (list (list "fcTL" '(4))))
	(list
	  nil
	  (list (list "fcTL" '(4)))))

  (check-expect
    	(getChunksWithName "IHDR" nil)
	(list
	  nil
	  nil))

  (check-expect
    	(takeChunk "IHDR" (list (list "IHDR" '(42))
		(list "fcTL" '(4))
		(list "fdAT" '(8))
		(list "IHDR" '(43))))
	(list
	  (list "IHDR" '(42))
	  (list	(list "fcTL" '(4))
	        (list "fdAT" '(8))
                (list "IHDR" '(43)))))

  (check-expect
    	(takeChunk "IHDR" (list (list "fcTL" '(4))))
	nil)

)

(link Test
  (MpngUtils MapngExploder))

(invoke Test)
