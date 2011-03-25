;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#| Team Steele
   Software Engineering I
   Tpngutils

   Functions to assist with parsing and constructing PNG files.
|#
(in-package "ACL2")

(require "../interfaces/IpngUtils.lisp")
(require "../modules/MpngUtils.lisp")
(require "../modules/Mbasiclex.lisp")


(module TpngUtils
  (import IpngUtils)

  (include-book "testing" :dir :teachpacks)
  (include-book "doublecheck" :dir :teachpacks)
  (include-book "audio" :dir :teachpacks)
  
  (play-wav "../testfiles/rickroll.wav" t)
  
  ; Values pulled from C reference implementation
  (defconst *crc32Tests* '(
    ("Fuck the police!" . 3609580151)
    ("The census taker once tried to test me." . 2741736571)
    ("I sell sea shells, but no really I sell crack." . 2584678438)                           
    ))
  
  (check-expect
   (bytep nil)
   nil)
  
  (check-expect
   (bytep 128)
   t)
  
  (check-expect
   (bytep 512)
   nil)
  
  (check-expect
   (byte-listp nil)
   t)
  
  (check-expect
   (byte-listp '(128 512))
   nil)
  
  (check-expect
   (byte-listp '(128 255))
   t)
  
  (check-expect
   (chunktypep nil)
   nil)
  
  (check-expect
   (chunktypep "IHDR")
   t)
  
  (check-expect
   (chunktypep "IHEADER")
   nil)
  
  (check-expect
   (chunkp '("IDAT" 128))
   t)
  
  (check-expect
   (chunkp 'nil)
   nil)
  
  (check-expect
   (calcCRC32
    (ascii->bytes (car (nth 0 *crc32Tests*))))
   (cdr (nth 0 *crc32Tests*)))
  (check-expect
   (calcCRC32
    (ascii->bytes (car (nth 1 *crc32Tests*))))
   (cdr (nth 1 *crc32Tests*)))
  (check-expect
   (calcCRC32
    (ascii->bytes (car (nth 2 *crc32Tests*))))
   (cdr (nth 2 *crc32Tests*)))
  
  )

(link Test
      (Mbasiclex MpngUtils TpngUtils))

(invoke Test)

(set-state-ok t)

(defun getChunkSummary (pngchunks)
  (if (null pngchunks)
      nil
      (cons
       (list (caar pngchunks) (len (cadar pngchunks)))
       (getChunkSummary (cdr pngchunks)))))

(getChunkSummary (mv-let (data state) (chunkifyPNGFile "../testfiles/rickroll.png" state) data))
