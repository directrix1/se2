;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.ss" ("cce" "dracula.plt") "modular" "lang")
;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
;; #reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#|
   Team Steele
   Software Engineering II
   IapngExploder

   Takes an APNG file and explodes it into PNG files.
|#

;Helper Function
(defun listofmv-check-len (input length)
    (if (null input)
        't
        (if (equal length (len (car input)))
            (listofmv-check-len (cdr input) length)
            'nil)))
  

(interface IapngExploder

  ; Finds the first data chunk in a ((name chunkdata) (name chunkdata))
  ; list that matches the given name and pulls it out of the chunklist
  (sig takeChunk (name chunklist checkedchunks))

  ; Given a list of chunks (chunkstocheck), iterate through them and
  ; build:
  ;   a) a list of chunks that match name
  ;   b) a a list of chunks that don't match name
  ; The function delivers a two-element list where the first element is
  ; the list described in (a) and the second is the list described in (b).
  (sig getChunksWithName (name chunkstocheck))

  ; Given an APNG file, breaks the APNG into its constituent PNG Images.
  ; This process involves looking at the acTL chunk to determine number of
  ; frames and number of plays, as well as looking at the fcTL and fdAT
  ; pairs to reconstruct the IHDR and IDAT chunks of the PNG Images that
  ; comprise the APNG input.
  ; Output is as follows:
  ;	APNG → (	numFrames numPlays 
  ;			(framedata1 time_for_frame1)
  ;			(framedata2 time_for_frame2)
  ;			… 					)
  ; apngdata = raw apng data string given from the IO Module.
  ; Output is a list of lists: (file-data, file-name)
  (sig explodeAPNG (apngdata))

  ; Given APNG chunks, returns the IHDR chunk contained within.
  ; chunks = processed (or raw) data chunks contained within the input APNG
  (sig getIHDR (chunks))
  
  ; Given APNG chunks, returns the numFrames and numPlays contained within
  ; the acTL chunk following the IHDR chunk. These two elements completely
  ;   comprise the acTL chunk.
  ; chunks = processed (or raw) data chunks contained within the input APNG
  (sig getacTL (chunks))
  
  ; Given the APNG chunks from input, the last found fcTL chunk, and the
  ; IHDR chunk for the APNG, constructs from the next fdAT chunk the PNG
  ; Signature, IHDR, IDAT, and IEND chunks necessary to create a complete
  ; PNG file. 
  ; chunks = processed (or raw) data chunks contained within the input APNG
  ; IDATflag = Defines whether or not the IDAT chunks have been pulled yet
  ; ihdr = ihdr for entire APNG file, data contained herein will be used to
  ;   reconstruct all PNG files
  (sig getFrames (chunks IDATflag prefix ihdr))
  
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;; Contracts
  (con takeChunk-always-finds-first
       (implies (and (stringp name) (stringp data1) (stringp data2) 
		     (stringp data3))
	(let* ((onel (list name data1))
	       (twol (list name data2))
	       (threel (list name data3))
	       (chunkl (list onel twol threel)))
			(equal (car (takeChunk name chunkl nil)) onel))))

  (con getChunksWithName-always-finds-all
       (implies (and (stringp name) (stringp data1) (stringp data2)
	             (stringp data3)) 
	(let* ((onel (list name data1))
	       (twol (list name data2))
	       (threel (list name data3))
	       (chunkl (list onel twol threel)))
			(equal chunkl (car 
				(getChunksWithName name chunkl))))))

  (con explodeAPNG-returns-null-or-mvs
       (implies (stringp input)
                (let ((output (explodeAPNG input)))
                  (or (null output)
                      (and (listp output)
                           (listofmv-check-len 2 output))))))
  
  (con getIHDR-returns-string-or-null
       (implies (stringp input)
                (let ((output (getIHDR input)))
                  (or (null output)
                      (stringp output)))))
  
  (con getacTL-returns-string-or-null
       (implies (stringp input)
                (let ((output (getacTL input)))
                  (or (null output)
                      (stringp output)))))
  
  (con getFrames-returns-string-or-null
       (implies (and (stringp chunks)
                     (stringp lastFCTL)
                     (stringp ihdr))
                (let ((output (getFrames chunks lastFCTL ihdr)))
                  (or (null output)
                      (stringp output)))))
  
  )
