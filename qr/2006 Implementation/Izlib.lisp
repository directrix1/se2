;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#| Team Steele
   Software Engineering I
   Izlib

   Functions to work with a zlib datastream.
|#
(in-package "ACL2")

(interface Izlib
  ; Returns a byte list representing bytes compressed into the zlib format
  ; where:
  ;  bytes = a list of byte values (0 -> 255) which represent the
  ;           data to be compressed into zlib format.
  ;  compresslevel = a number representing the requested compression level 
  ;                   (0 fast/least compressed -> 9 slow/most compressed)
  (sig zlib-compress (bytes compresslevel))

  ; Decompresses a zlib datastream to it's representative byte list or
  ; returns nil on failure where:
  ;  bytes = a list of byte values (0 -> 255) which represent
  ;           the zlib datastream.
  (sig zlib-decompress (bytes))
  
  ; Contracts =============================================================
)
