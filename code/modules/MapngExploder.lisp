;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.ss" ("cce" "dracula.plt") "modular" "lang")
;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
;; #reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")

(require "../interfaces/Ibasiclex.lisp")
(require "../interfaces/IxmlUtils.lisp")
(require "../interfaces/IminidomSerializer.lisp")
(require "../interfaces/IminidomParser.lisp")
(require "../interfaces/IapngExploder.lisp")

#|
   Team Steele
   Software Engineering II
   IapngExploder

   Takes an APNG file and explodes it into PNG files.
|#

(module MapngExploder
  (include-book "list-utilities" :dir :teachpacks)
  
  ; Given an APNG file, breaks the APNG into its constituent PNG Images.
  ; This process involves looking at the acTL chunk to determine number of
  ; frames and number of plays, as well as looking at the fcTL and fdAT
  ; pairs to reconstruct the IHDR and IDAT chunks of the PNG Images that
  ; comprise the APNG input.
  ; Output is as follows:
  ;	APNG → (	numFrames numPlays 
  ;			(framedata1 time_for_frame1)
  ;			(framedata2 time_for_frame2)
  ;			... 					)
  ; apngdata = raw apng data string given from the IO Module.
  ; Output is a list of lists: (file-data, file-name)
  (defun explodeAPNG (apngdata) 
    nil)

  ; Given APNG chunks, returns the IHDR chunk contained within.
  ; chunks = processed (or raw) data chunks contained within the input APNG
  (defun getIHDR (chunks) 
    nil)
  
  ; Given APNG chunks, returns the numFrames and numPlays contained within
  ; the acTL chunk following the IHDR chunk. These two elements completely
  ;   comprise the acTL chunk.
  ; chunks = processed (or raw) data chunks contained within the input APNG
  (defun getacTL (chunks) 
    nil)
  
  ; Given the APNG chunks from input, the last found fcTL chunk, and the
  ; IHDR chunk for the APNG, constructs from the next fdAT chunk the PNG
  ; Signature, IHDR, IDAT, and IEND chunks necessary to create a complete
  ; PNG file. 
  ; chunks = processed (or raw) data chunks contained within the input APNG
  ; lastFCTL = last found fcTL chunk, or the fcTL for which the next fdAT
  ;   chunk is defined
  ; ihdr = ihdr for entire APNG file, data contained herein will be used to
  ;   reconstruct all PNG files
  (defun getFrames (chunks lastFCTL ihdr) 
    nil)
  
 (export IapngExploder))