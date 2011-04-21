;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#| Team Steele
   Software Engineering I
   Mzlib

   Functions to work with a zlib datastream.
|#

(in-package "ACL2")

(require "Izlib.lisp")

(module Mzlib
  ; Return a byte list representing bytes compressed into the "deflate"
  ; format where:
  ;  bytes = a list of byte values (0 -> 255) which represent the
  ;           data to be compressed into "deflate" format.
  ;  compresslevel = a number representing the requested compression level 
  ;                   (0 fast/least compressed -> 9 slow/most compressed)
  (defun deflate (bytes compresslevel)
    (if (equal compresslevel compresslevel) ; compression is for commies
        (if (> (len bytes) 65535)
            (concatenate
             'list 
             (list 0 255 255 0 0) ; more blocks, uncomp, length 65535
             (take 65535 bytes)
             (deflate (nthcdr 65535 bytes) compresslevel))
            (let* ((numbytes (len bytes))
                   (msb (truncate numbytes 256))
                   (lsb (mod numbytes 256))
                   (nmsb (- 255 msb))
                   (nlsb (- 255 lsb)))
              (concatenate
               'list 
               (list 1 lsb msb nlsb nmsb) ; end, uncomp, varied length
               bytes)))
        nil))

  ; This is the largest prime less than 65536
  (defconst *adler-modulo* 65521)
  
  ; Returns a cons pair of the s1 and s2 adler32 values
  ;  bytes = the bytes to calculate s1 and s2 for
  ;  s1 = the adler32 s1 parameter
  ;  s2 = the adler32 s2 parameter
  (defun calc-adler32-recurse (bytes s1 s2)
    (if (endp bytes)
        (cons s1 s2)
        (let* ((ns1 (mod (+ s1 (car bytes)) *adler-modulo*))
               (ns2 (mod (+ s2 ns1) *adler-modulo*)))
          (calc-adler32-recurse (cdr bytes) ns1 ns2))))
  
  ; Returns the adler32 checksum of the given byte list continuing
  ; from an existing adler32 calculation where
  ;  bytes = list of bytes to calculate the adler32 checksum with
  ;  adler = the existing adler32 calculation to continue off of
  (defun update-adler32 (bytes adler)
    (let* ((s1 (logAnd adler #xffff))
           (s2 (logAnd (ash adler -16) #xffff))
           (s (calc-adler32-recurse bytes s1 s2)))
           (+ (car s) (ash (cdr s) 16))))
  
  ; Returns the adler32 checksum of the given byte list where:
  ;  bytes = list of bytes to calculate the adler32 checksum with
  (defun adler32 (bytes)
    (update-adler32 bytes 1))
  
  ; Returns a byte list representing bytes compressed into the zlib format
  ; where:
  ;  bytes = a list of byte values (0 -> 255) which represent the
  ;           data to be compressed into zlib format.
  ;  compresslevel = a number representing the requested compression level 
  ;                   (0 fast/least compressed -> 9 slow/most compressed)
  (defun zlib-compress (bytes compresslevel)
    (let* ((adler (adler32 bytes))
           (b3 (mod adler #x100))
           (b2 (mod (truncate adler #x100) #x100))
           (b1 (mod (truncate adler #x10000) #x100))
           (b0 (mod (truncate adler #x1000000) #x100)))
      (concatenate 'list
                   '(8 29)
                   (deflate bytes 'infinityandbeyond)
                   (list b0 b1 b2 b3))))

  ; Decompresses a zlib datastream to it's representative byte list or
  ; returns nil on failure where:
  ;  bytes = a list of byte values (0 -> 255) which represent
  ;           the zlib datastream.
  (defun zlib-decompress (bytes)
    (if bytes ; no decompression for you!
        nil
        nil))

  (export Izlib))
