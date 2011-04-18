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

(require "../interfaces/IapngExploder.lisp")
(require "../interfaces/IpngUtils.lisp") ; XXX needed?
(require "../modules/MapngExploder.lisp")
(require "../modules/MpngUtils.lisp") ; XXX needed?



(defconst *APNG* 
  (list	(list "IHDR" (list 0 0 0 1 0 0 0 1 8 2 0 0 0))
        (list "acTL" 
              (list 0 0 0 3 0 0 0 0))
        (list "fcTL" 
              (list 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 0 0))
        (list "IDAT" 
              (list 120 218 98 98 96 96 0 8 48 0 0 12 0 3))
        (list "fcTL" 
              (list 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 0 0))
        (list "fdAT" 
              (list 0 0 0 2 120 218 98 98 96 96 0 8 48 0 0 12 0 3))
        (list "fcTL" 
              (list 0 0 0 3 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 0 0))
        (list "fdAT" 
              (list 0 0 0 4 120 218 98 98 96 96 0 8 48 0 0 12 0 3))
        (list "IEND" 
              nil)))

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
    (splitAtFirstFrameChunk nil nil 
                            (list 
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
                            (list "IHDR" '(43)))
               nil)
    (list
      (list "IHDR" '(42))
      (list	  (list "fcTL" '(4))
                  (list "fdAT" '(8))
                  (list "IHDR" '(43)))))
  
  (check-expect
    (takeChunk "IHDR" (list (list "fcTL" '(4))) nil)
    nil)
  
  (check-expect
    (explodeAPNG (makeChunks *APNG*))
    (list "3" "0" (list 
                   (list (list 137 80 78 71 13 10 26 10 0 0 0 13 73 72 68
                               82 0 0 0 1 0 0 0 1 8 2 0 0 0 144 119 83 222
                               0 0 0 14 73 68 65 84 120 218 98 98 96 96 0
                               8 48 0 0 12 0 3 35 114 99 92 0 0 0 26 102
                               99 84 76 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0
                               0 0 0 0 1 0 1 0 0 86 42 223 49 0 0 0 0 73 
                               69 78 68 174 66 96 130) "1/1") 
                   (list (list 137 80 78 71 13 10 26 10 0 0 0 13 73 72 68
                               82 0 0 0 1 0 0 0 1 8 2 0 0 0 144 119 83 222
                               0 0 0 14 73 68 65 84 120 218 98 98 96 96 0
                               8 48 0 0 12 0 3 35 114 99 92 0 0 0 26 102
                               99 84 76 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 0 
                               0 0 0 0 0 1 0 1 0 0 205 89 53 229 0 0 0 0
                               73 69 78 68 174 66 96 130) "1/1")
                   (list (list 137 80 78 71 13 10 26 10 0 0 0 13 73 72 68
                               82 0 0 0 1 0 0 0 1 8 2 0 0 0 144 119 83 222
                               0 0 0 14 73 68 65 84 120 218 98 98 96 96 0
                               8 48 0 0 12 0 3 35 114 99 92 0 0 0 26 102
                               99 84 76 0 0 0 3 0 0 0 1 0 0 0 1 0 0 0 0
                               0 0 0 0 0 1 0 1 0 0 32 207 230 12 0 0 0 0
                               73 69 78 68 174 66 96 130) "1/1"))))
  )

(link Test
      (MpngUtils MapngExploder))

(invoke Test)

