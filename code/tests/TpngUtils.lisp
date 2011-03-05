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
  
  (defun chars->bytes (chars)
    (if (null chars)
        nil
        (cons (char-code (car chars))
              (chars->bytes (cdr chars)))))

  (defun string->bytes (string)
    (chars->bytes (coerce string 'list)))
  
  ; Values pulled from C reference implementation
  (defconst *crc32Tests* '(
    ("Fuck the police!" . 3609580151)
    ("The census taker once tried to test me." . 2741736571)
    ("I sell sea shells, but no really I sell crack." . 2584678438)                           
    ))
  (check-expect
   (calcCRC32
    (string->bytes (car (nth 0 *crc32Tests*))))
   (cdr (nth 0 *crc32Tests*)))
  (check-expect
   (calcCRC32
    (string->bytes (car (nth 1 *crc32Tests*))))
   (cdr (nth 1 *crc32Tests*)))
  (check-expect
   (calcCRC32
    (string->bytes (car (nth 2 *crc32Tests*))))
   (cdr (nth 2 *crc32Tests*)))
  
  ;  (defun byte-bits (byte)
    
  
  ;  (defun bytes-bits (bytes)
  ;    (if (null bytes)
  ;        nil
  ;        (cons 

  )

(link Test
      (Mbasiclex MpngUtils TpngUtils))

(invoke Test)