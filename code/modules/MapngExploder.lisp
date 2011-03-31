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
  ; list that matches the given name and pulls it out of the chunklist
  (defun takeChunk (name chunklist checkedchunks)
    (if (null chunklist)
        nil
        (if (equal (caar chunklist) name)
            (list (append checkedchunks (cdr chunklist))
                  (cadar chunklist))
            (takeChunk name 
                       (cdr chunklist)
                       (append checkedchunks (car chunklist))))))
  
  
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
  
  ; Given APNG chunks, returns the IHDR chunk contained within.
  ; chunks = processed (or raw) data chunks contained within the input APNG
  (defun getIHDR (chunks) 
    (if (and (eq (caar chunks) "IHDR")
             (chunkp (cadr (car chunks))))
        (cadr (car chunks))
        nil))
  
  ; Given APNG chunks, returns the numFrames and numPlays contained within
  ; the acTL chunk following the IHDR chunk. These two elements completely
  ;   comprise the acTL chunk.
  ; chunks = processed (or raw) data chunks contained within the input APNG
  (defun getAcTL (chunks) 
    (let* ((actl (takeChunk "acTL" chunks nil))
           (numFrames (parseNum (take 4 actl) nil 4))
           (numPlays (parseNum (take 4 (nthcdr 4 actl)) nil 4)))
      (list numFrames numPlays)))
  
  ; Takes out the extra chunks that come before the next certain chunk
  ; If the IDATflag is true, it stops at IDAT, fcTL, and IEND chunks
  ; else it stops at only the fcTL or IEND chunks
  (defun cleanChunks (IDATflag pre post)
    (if (or (null post)
            (equal (caar post) "fcTL")
            (equal (caar post) "IEND")
            (if IDATflag 
                (equal (caar post) "IDAT")
                nil))
        (list pre post)
        (cleanChunks IDATflag (append pre (car post)) (cdr post))))
   
  ; Makes all the chunks in the list of chunks and concatenates them
  (defun makeChunks (chunklist)
    (if (null chunklist)
        nil
        (append (makeChunk (caar chunklist) (cadar chunklist))
                (makeChunks (cdr chunklist)))))
  
  ; Given the APNG chunks from input, the last found fcTL chunk, and the
  ; IHDR chunk for the APNG, constructs from the next fdAT chunk the PNG
  ; Signature, IHDR, IDAT, and IEND chunks necessary to create a complete
  ; PNG file. 
  ; chunks = processed (or raw) data chunks contained within the input APNG
  ; IDATflag = Defines whether or not the IDAT chunks have been pulled yet  
  ; prefix = ihdr and other chunks for entire APNG file, data contained 
  ; herein will be used to reconstruct all PNG files.
  (defun getFrames (chunks IDATflag prefix) 
    (let* ()
      ()))
     
  (defun getFrame (chunks IDATflag prefix)
     (let* ((seperate (cleanChunks nil nil chunks))
            (extrachunks (makeChunks (car seperate)))
            (clean (cadr seperate))
            (imgdata nil) ;Compiled Image Data
            (IDAT (makeChunk "IDAT" imgdata)) ;IDAT
            ;Other Chunks after the DAT
            (IEND (makeChunk "IEND" nil)) ;IEND
            ;Frame Delay
            (fctl (takeChunk "fcTL" chunks nil))
            (fdnum (parsenum (take 2 (nthcdr 20 fctl)) nil 2))
            (fddenom (parsenum (take 2 (nthcdr 22 fctl)) nil 2))
            (framedelay (concatenate 'string 
                                     (rat->str fdnum 0) "/"
                                     (rat->str fddenom 0))))
       (list (concatenate 'list
                       prefix ;IHDR and other chunks before fcTL/DATs
                       IDAT   ;IDAT for this frame
                       IEND)  ;IEND
             framedelay))) 

                       
          
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
           (seperate (cleanChunks 't nil chunks))
           (extra (makeChunks (car seperate)))
           (prefix (append ihdr extra))
           (clean (cadr seperate))
           (frames (getFrames clean 't prefix)))
      (append actl frames)))

  
 (export IapngExploder))