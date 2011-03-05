;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#| Team Steele
   Software Engineering I
   Mpngutils

   Functions to assist with parsing and constructing PNG files.
|#
(in-package "ACL2")

(require "../interfaces/IpngUtils.lisp")
(require "../interfaces/Ibasiclex.lisp")

(module MpngUtils
  (include-book "list-utilities" :dir :teachpacks)
  
  ; After being given a lot of PNG image data, blowChunks processes this
  ; data on a chunk by chunk basis and subsequently returns the list of
  ; list pairs of chunk type and chunk data.
  ;	For example:
  ;	*PNG Image → 	(  (list IHDR ihdr_data) (list IDAT idat_data) 
  ;					... 			..	)
  ;	*APNG Image → 	(  (list IHDR ihdr_data) (list acTL actl_data) 
  ;			   	(list fcTL fctl_1)    (list fdAT fdat_1)
  ;				(list fcTL fctl_2)    (list fdAT fdat_2)
  ;					...		        ..	)
  ;	pngdata = raw, unprocessed png data bytes
  (defun blowChunks (pngdata) pngdata)
  
  ; Given a chunk type and correctly formatted chunkdata, makeChunk returns
  ; the correctly formatted chunk including the chunk length, type, data,
  ; and CRC (using calcCRC32).
  ;  chunktype = type of the chunk to be created
  ;  chunkdata = raw data portion of the chunk to be created
  (defun makeChunk (chunktype chunkdata) (list chunktype chunkdata))
  
  ; Converts the given number into an unsigned int or other options to be
  ; used in chunk processing / creating.
  ;  num = number to be converted 
  ;  signed = true means make it two's complement
  ;  numbytes = number of bytes used in representation
  (defun makeNum (num signed numbytes)
    (if (zp numbytes)
        nil
        (let ((thenum
               (if (and signed (< num 0))
                   (+ (ash 1 (* 8 numbytes)) num)
                   num)))
          (cons 
           (ash thenum (* 8 (- 1 numbytes)))
           (makeNum (logand num (- (ash 1 (* 8 (- numbytes 1))) 1)) nil (- numbytes 1))))))
  
  ; Parses a number given in a non-standard type.
  ;  bytes = number to be parsed
  ;  signed = true means make it two's complement
  ;  numbytes = number of bytes used in representation
  (defun parseNum (bytes signed numbytes)
    (if (zp numbytes)
        0
        (let ((thenum (+ 
                       (ash (car bytes) (* 8 (- numbytes 1)))
                       (parseNum (cdr bytes) nil (- numbytes 1)))))
          (if (and signed (> (car bytes) 127))
              (- thenum (ash 1 (* 8 numbytes)))
              thenum))))
  
  ; Given raw data bytes, such as that found in the data portion of a
  ; PNG Image chunk, returns the calculated CRC.
  ;  bytes = raw data from PNG Image or other source
  (defun calcCRC32 (bytes) bytes)
  
(export IpngUtils))