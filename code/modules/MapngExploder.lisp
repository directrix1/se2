;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.ss" ("cce" "dracula.plt") "modular" "lang")
;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
;; #reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
(in-package "ACL2")

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
  
  
  (include-book "list-utilities" :dir :teachpacks)
  (include-book "io-utilities" :dir :teachpacks)
  (import IpngUtils)
  
  ; The signature that goes at the beginning of every png file
  (defconst *pngsig*
    (list 137 80 78 71 13 10 26 10))

  ; Finds the first data chunk in a ((name chunkdata) (name chunkdata))
  ; list that matches the given name and pulls it out of the chunklist
  (defun takeChunk (name chunklist checkedchunks)
    (if (null chunklist)
        nil
        (if (equal (caar chunklist) name)
            (list (cadar chunklist)
                  (append checkedchunks (cdr chunklist)))
            (takeChunk name 
                       (cdr chunklist)
                       (append checkedchunks (list (car chunklist)))))))

  ; Given a list of chunks (chunkstocheck), iterate through them and
  ; build:
  ;   a) a list of chunks that match name
  ;   b) a a list of chunks that don't match name
  ; The function delivers a two-element list where the first element is
  ; the list described in (a) and the second is the list described in (b).
  (defun getChunksWithName (name chunkstocheck)
    (if (null chunkstocheck)
      ; XXX Also need to check (specify in the contract?) that if
      ; chunkstocheck *is* nonempty that it's also a list that matches
      ; the form delivered by blowChunks.
      (list nil nil)
      (let* ((currentname (caar chunkstocheck))
             (result (getChunksWithName name (cdr chunkstocheck)))
             (matches (car result))
             (nonmatches (cadr result)))
        (if (equal currentname name)
            (list (append (list (car chunkstocheck)) matches) nonmatches)
            (list matches (append (list (car chunkstocheck)) nonmatches))))))
  
  ; Given APNG chunks, returns the IHDR chunk contained within.
  ; chunks = processed (or raw) data chunks contained within the input APNG
  (defun getIHDR (chunks) 
    (if (equal (caar chunks) "IHDR")
        (cadar chunks)
        nil))
  
  ; Given APNG chunks, returns the numFrames and numPlays contained within
  ; the acTL chunk following the IHDR chunk. These two elements completely
  ;   comprise the acTL chunk.
  ; chunks = processed (or raw) data chunks contained within the input APNG
  (defun getAcTL (chunks) 
    (let* ((actl (car (takeChunk "acTL" chunks nil)))
           (numFrames (rat->str (parseNum (take 4 actl) nil 4) 0))
           (numPlays (rat->str (parseNum (take 4 (nthcdr 4 actl)) nil 4) 0)))
      (list numFrames numPlays)))
  
  ; Takes out the extra chunks that come before the next certain chunk
  ; If the IDATflag is true, it stops at IDAT, fcTL, and IEND chunks
  ; else it stops at only the fcTL or IEND chunks
  (defun splitAtFirstFrameChunk (IDATflag pre post)
    (if (or (null post)
            (equal (caar post) "fcTL")
            (equal (caar post) "IEND")
            (and IDATflag (equal (caar post) "IDAT")))
            (list pre post)
        (splitAtFirstFrameChunk IDATflag
                                (append pre (list (car post)))
                                (cdr post))))
   
  ; Makes all the chunks in the list of chunks and concatenates them
  (defun makeChunks (chunklist)
    (if (null chunklist)
        nil
        (append (makeChunk (caar chunklist) (cadar chunklist))
                (makeChunks (cdr chunklist)))))
  
  ; Splits the chunklist on the next fcTL
  (defun splitByFcTL (pre post)
    (if (null post)
        (list pre post)       
        (if (or (equal 1 (len post))
                (equal "IEND" (caadr post))
                (equal "fcTL" (caadr post)))
            (list (append pre (list (car post))) (cdr post))
            (splitByFcTL (append pre (list (car post))) (cdr post)))))
  
  ; This function formats the raw PNG file data into more conveniently 
  ; utilized chunks, and returns (list fdat IDAT) where IHDR is the IHDR
  ; chunk IDAT is all IDAT chunks concatenated into one chunk.
  ; fdat = fdat chunk data, pass in nil initially
  ; idat = idat chunk data, pass in nil initially
  ; chunks = a list of blown chunks
  (defun buildDataChunk (fdat idat chunks)
    (if (null chunks)
        (list fdat idat)
        (let* ((curchunk (car chunks))
               (rest (cdr chunks))
               (chunkname (car curchunk))
               (chunkbytes (cadr curchunk)))
          (cond
            ((equal chunkname "fdAT") (buildDataChunk
                                       (concatenate 'list 
                                                    fdat
                                                    (nthcdr 4 chunkbytes))
                                       idat
                                       rest))
            ((equal chunkname "IDAT") (buildDataChunk
                                       fdat
                                       (concatenate 'list idat chunkbytes)
                                       rest))
            ('t (buildDataChunk fdat idat rest))))))
  
  ; Given the APNG chunks from input, the last found fcTL chunk, and the
  ; IHDR chunk for the APNG, constructs from the next fdAT chunk the PNG
  ; Signature, IHDR, IDAT, and IEND chunks necessary to create a complete
  ; PNG file. 
  ; chunks = processed (or raw) data chunks contained within the input APNG
  ; IDATflag = Defines whether or not the IDAT chunks have been pulled yet
  ; prefix = ihdr and other chunks for entire APNG file, data contained 
  ; herein will be used to reconstruct all PNG files.
  ; ihdr = the IHDR chunk not passed through makeChunk
  (defun getFrame (chunks IDATflag prefix ihdr)
     (let* ((seperate (if IDATflag
                          (getChunksWithName "IDAT" chunks)
                          (getChunksWithName "fdAT" chunks)))
            ;(test (if (not IDATflag)
             ;         (coerce (list(car chunks)) 'string) ;:msg (msg "hi"))
              ;        nil))
            (extrachunks (makeChunks (cadr seperate)))
            (clean (car seperate))
            (imgdata (if IDATflag  ; Compiled image data
                         (cadr (buildDataChunk nil nil clean))
                         (car (buildDataChunk nil nil clean))))
            (IDAT (makeChunk "IDAT" imgdata)) ;IDAT
            ;Other Chunks after the DAT
            (IEND (makeChunk "IEND" nil)) ;IEND
            ;Frame Delay
            (fctl (car (takeChunk "fcTL" chunks nil))))
       (if (null fctl)
           (list (concatenate 'list
                               *pngsig*
                               (makeChunk "IHDR" ihdr) ;IHDR
                               prefix ;other chunks
                               IDAT   ;IDAT for this frame
                               extrachunks ;more other chunks
                               IEND);IEND
                   "-1/1")
           
           (let* ((width   (parsenum (nthcdr 4 fctl)  nil 4))
                  (height  (parsenum (nthcdr 8 fctl)  nil 4))
                  (fdnum   (parsenum (nthcdr 20 fctl) nil 2))
                  (fddenom (parsenum (nthcdr 22 fctl) nil 2))
                  (framedelay (concatenate 'string 
                                (rat->str fdnum 0) "/"
                                (rat->str fddenom 0))))
             
             (list (concatenate 'list
                                *pngsig*
                                (makeChunk "IHDR" (concatenate 'list
                                                   (makeNum width nil 4)
                                                   (makeNum height nil 4)
                                                   (nthcdr 8 ihdr)))
                                prefix ;other chunks
                                IDAT   ;IDAT for this frame
                                extrachunks
                                IEND)  ;IEND
                   framedelay)))))

  (defun getFrames (chunks IDATflag prefix ihdr) 
    (if (or (null chunks)
            (equal "IEND" (caar chunks)))
        nil
        (let ((split (splitbyfcTL nil chunks)))
          (append (list (getFrame (car split) IDATflag prefix ihdr))
                (getFrames (cadr split) nil prefix ihdr)))))     
                       
          
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
           (ihdr (getIHDR chunks))
           (actl (getAcTL (cdr chunks)))
           (seperate (splitAtFirstFrameChunk 
                      't nil (cadr (takechunk "acTL" (cdr chunks) nil))))
           (prefix (makeChunks (car seperate)))
           (clean (cadr seperate))
           (frames (getFrames clean 't prefix ihdr)))
           ;(frames (splitByFcTL nil clean)))
           ;(frames (getFrame clean 't nil ihdr)))
      (append actl (list frames))))

  
 (export IapngExploder))
