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
(require "../interfaces/IpngUtils.lisp")

#|
   Team Steele
   Software Engineering II
   IapngExploder

   Takes an APNG file and explodes it into PNG files.
|#

(module MapngExploder
  (in-package "ACL2")
  
  (include-book "list-utilities" :dir :teachpacks)
  (include-book "io-utilities" :dir :teachpacks)
  (import IpngUtils)
  
  
  ; Finds the first data chunk in a ((name chunkdata) (name chunkdata))
  ; list that matches the given name
  (defun findChunk (name chunklist)
    (if (null chunklist)
        nil
        (if (equal (caar chunklist) name)
            (cadar chunklist)
            (findChunk name (cdr chunklist)))))
  
  
  ; Helps findConsecChunks by finding only the consecutive chunks after the
  ; first one.
  (defun findConsecRest (name chunklist)
    (if (null chunklist)
        nil
        (if (equal (caar chunklist) name)
            (append (cadar chunklist)
                    (findConsecRest name (cdr chunklist))) 
            nil)))
  
  ; Finds all consecutive data chunks in a list of (name chunkdata)'s
  ; that match the given name
  (defun findConsecChunks (name chunklist)
    (if (null chunklist)
        nil
        (if (equal (caar chunklist) name)
            (append (cadar chunklist)
                    (findConsecRest name (cdr chunklist)))                    
            (findConsecChunks name (cdr chunklist)))))
  
  ; Given an APNG file, breaks the APNG into its constituent PNG Images.
  ; This process involves looking at the acTL chunk to determine number of
  ; frames and number of plays, as well as looking at the fcTL and fdAT
  ; pairs to reconstruct the IHDR and IDAT chunks of the PNG Images that
  ; comprise the APNG input.
  ; Output is as follows:
  ;	APNG → (	numFrames numPlays 
  ;			((framedata1 time_for_frame1)
  ;			 (framedata2 time_for_frame2)
  ;			... 					))
  ; apngdata = raw apng data string given from the IO Module.
  (defun explodeAPNG (apngdata) 
    (let* ((chunks (blowChunks apngdata))
           (ihdr (makeChunk "IHDR" (getIHDR chunks)))
           (actl (getAcTL (cdr chunks)))
           (frames (getFrames chunks 't ihdr)))
      (append actl (list frames))))

  ; Given APNG chunks, returns the IHDR chunk contained within.
  ; chunks = processed (or raw) data chunks contained within the input APNG
  (defun getIHDR (chunks) 
    (if (and (eq (caar chunks) 'IHDR)
             (chunkp (cadr (car chunks))))
        (cadr (car chunks))
        nil))
  
  ; Given APNG chunks, returns the numFrames and numPlays contained within
  ; the acTL chunk following the IHDR chunk. These two elements completely
  ;   comprise the acTL chunk.
  ; chunks = processed (or raw) data chunks contained within the input APNG
  (defun getAcTL (chunks) 
    (let* ((actl (findChunk 'ACTL chunks))
           (numFrames (parseNum (take 4 actl) nil 4))
           (numPlays (parseNum (take 4 (nthcdr 4 actl)) nil 4)))
      (list numFrames numPlays)))
  
  ; Given the APNG chunks from input, the last found fcTL chunk, and the
  ; IHDR chunk for the APNG, constructs from the next fdAT chunk the PNG
  ; Signature, IHDR, IDAT, and IEND chunks necessary to create a complete
  ; PNG file. 
  ; chunks = processed (or raw) data chunks contained within the input APNG
  ; lastFCTL = last found fcTL chunk, or the fcTL for which the next fdAT
  ;   chunk is defined
  ; ihdr = ihdr for entire APNG file, data contained herein will be used to
  ;   reconstruct all PNG files
  (defun getFrames (chunks IDATflag ihdr) 
    (list (concatenate 'list 
                       (makeChunk "IHDR" ihdr)
                       (if (null IDATflag)
                           nil
                           nil)
                       (makeChunk "IEND" nil))
          (parsenum (take 2 (nthcdr 20 fctl)) nil 2)))
  
 (export IapngExploder))