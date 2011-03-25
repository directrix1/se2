;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#| Team Steele
   Software Engineering I
   MapngBuilder

   Module for building APNG files from PNG IHDR and IDAT chunks
|#
(in-package "ACL2")

(require "../interfaces/IapngBuilder.lisp")
(require "../interfaces/IpngUtils.lisp")

(module MapngBuilder
  (include-book "list-utilities" :dir :teachpacks)
  (include-book "io-utilities" :dir :teachpacks)
  
  (import IpngUtils)
  
  ; This function formats the raw PNG file data into more conveniently 
  ; utilized chunks, and returns (list IHDR IDAT) where IHDR is the IHDR
  ; chunk IDAT is all IDAT chunks concatenated into one chunk.
  ; ihdr = ihdr chunk data, pass in nil initially
  ; idat = idat chunk data, pass in nil initially
  ; framedata = a list of blown chunks
  (defun preparePNG (ihdr idat chunks)
    (if (null chunks)
        (list ihdr idat)
        (let* ((curchunk (car chunks))
               (rest (cdr chunks))
               (chunkname (car curchunk))
               (chunkbytes (cdr curchunk)))
          (cond
            ((equal chunkname "IHDR") (preparePNG
                                       (concatenate 'list ihdr chunkbytes)
                                       idat
                                       rest))
            ((equal chunkname "IDAT") (preparePNG
                                       ihdr
                                       (concatenate 'list idat chunkbytes)
                                       rest))
            ('t (preparePNG ihdr idat rest))
            ))))

  ; This function formats the raw PNG file data into more conveniently 
  ; utilized chunks, and returns a list of (list IHDR IDAT displaytime)
  ; where IHDR is the IHDR chunk IDAT is the IDAT chunk and time is the
  ; displaytime is the corresponding value from the framedata parameter.
  ; framedata = frame and time display information of the type 
  ;   (list (list frame displaytime)... ) where frame is a byte list
  ;   representing a PNG's contents and displaytime is a string
  ;   representing the amount of time in seconds to display that frame as
  ;   a rational number (i.e. 100/2997 for NTSC standard)
  (defun preparePNGs (framedata)
    (if (null framedata)
        nil
        (let* ((cur (car framedata))
               (rest (cdr framedata))
               (frame (preparePNG nil nil (car cur)))
               (time (cdr cur)))
          (cons
           (concatenate 'list frame (list time))
           (preparePNGs rest)))))
    
  ; Scan the list returned from preparePNGs for inconsistencies
  ; between PNG files' IHDR and the first frame's IHDR. APNG requires
  ; all frames to have the same compression, filter method, and bit depth,
  ; and for the width and height to be less than or equal to the first
  ; frame. This filter returns true if and only if all frames in prepdPNGs
  ; satisfy this property in relation to baseIHDR. If baseIHDR is null
  ; then the IHDR is extracted from the first PNG in the list and the
  ; function is recalled passing in this IHDR.
  ; prepdPNGs = prepared list of PNGs in the form described as the output
  ;    of preparePNGs
  ; baseIHDR = the reference IHDR or if null the IHDR of the first PNG
  ;    in the list to which all comparisons of consistency are made.
  (defun validateIHDR (prepdPNGs baseIHDR)
    (if (null prepdPNGs)
        nil
        (let* ((cur (car prepdPNGs))
              (rest (cdr prepdPNGs))
              (curIHDR (car cur)))
          (if (null baseIHDR)
              (validateIHDR rest curIHDR)
              (let* ((bWidth (parseNum baseIHDR nil 4))
                     (cWidth (parseNum curIHDR nil 4)))
                (if (not (= bWidth cWidth))
                    "Widths do not match"
              (let* ((b1 (nthcdr 4 baseIHDR))
                     (c1 (nthcdr 4 curIHDR))
                     (bHeight (parseNum b1 nil 4))
                     (cHeight (parseNum c1 nil 4)))
                (if (not (= bHeight cHeight))
                    "Heights do not match"
              (let* ((b2 (nthcdr 4 b1))
                     (c2 (nthcdr 4 c1))
                     (bBitDepth (parseNum b2 nil 1))
                     (cBitDepth (parseNum c2 nil 1)))
                (if (not (= bBitDepth cBitDepth))
                    "Bit depths do not match"
              (let* ((b3 (nthcdr 1 b2))
                     (c3 (nthcdr 1 c2))
                     (bColorType (parseNum b3 nil 1))
                     (cColorType (parseNum c3 nil 1)))
                (if (not (= bColorType cColorType))
                    "Color types do not match"
              (let* ((b4 (nthcdr 1 b3))
                     (c4 (nthcdr 1 c3))
                     (bCompression (parseNum b4 nil 1))
                     (cCompression (parseNum c4 nil 1)))
                (if (not (= bCompression cCompression))
                    "Compression methods do not match"
              (let* ((b5 (nthcdr 1 b4))
                     (c5 (nthcdr 1 c4))
                     (bFilter (parseNum b5 nil 1))
                     (cFilter (parseNum c5 nil 1)))
                (if (not (= bFilter cFilter))
                    "Filter methods do not match"
              (let* ((b6 (nthcdr 1 b5))
                     (c6 (nthcdr 1 c5))
                     (bInterlace (parseNum b6 nil 1))
                     (cInterlace (parseNum c6 nil 1)))
                (if (not (= bInterlace cInterlace))
                    "Interlace methods do not match"
                    nil
                    ))))))))))))))))))

  ; Returns a byte list representing the acTL chunk described by the
  ; parameters.
  ; numPlays = number of times the animation is intended to be played
  ; numFrames = the number of frames in the animation
  (defun buildACTL (numPlays numFrames)
      (makeChunk "acTL"
                 (concatenate 
                  'list
                  (makeNum numFrames nil 4)
                  (makeNum numPlays nil 4)
                  )))

  ; Returns a byte list representing the fcTL chunk described by the
  ; parameters.
  ; sequenceNum = the sequence number in the animation
  ; width = width of the frame
  ; height = height of the frame
  ; xOffset = xOffset of the frame
  ; yOffset = yOffset of the frame
  ; delayTime = frame delay string in the form of a rational number
  ;    (i.e. 100/2997) in seconds
  ; disposeOp = after display do 0 = nothing, 1 = transparent black,
  ;     2 = revert to previous frame
  ; blendOp = 0 = overwrite all color components including alpha,
  ;     1 = blend over
  (defun buildFCTL (sequenceNum width height xOffset yOffset delayTime
                  disposeOp blendOp)
    (let*
        ((delayT (str->rat delayTime))
         (delayNum (numerator delayT))
         (delayDen (denominator delayT)))
      (makeChunk "fcTL"
                 (concatenate 
                  'list
                  (makeNum sequenceNum nil 4)
                  (makeNum width nil 4)
                  (makeNum height nil 4)
                  (makeNum xOffset nil 4)
                  (makeNum yOffset nil 4)
                  (makeNum delayNum nil 2)
                  (makeNum delayDen nil 2)
                  (makeNum disposeOp nil 1)                
                  (makeNum blendOp nil 1)                
                  ))))

  ; Convert the prepdPNGs parameter into a byte list representation of the
  ; APNG frames with their associated fcTL, IDAT and fdAT chunks using 
  ; prepdPNGs as a source for the framedata. This does not include the 
  ; file signature, IHDR or acTL chunk.
  ; prepdPNGs = prepared list of PNGs in the form described as the output
  ;    of preparePNGs
  ; frameNum = if 0 then the image data of the first PNG of the list will
  ;    be output as an IDAT chunk all other frames are fdAT chunks
  (defun buildFrames (prepdPNGs frameNum)
    (if (null prepdPNGs)
        nil
        (let* ((cur (car prepdPNGs))
               (rest (cdr prepdPNGs))
               (IHDR (car cur))
               (width (parseNum (nthcdr 4 IHDR) nil 4))
               (height (parseNum (nthcdr 8 IHDR) nil 4))
               (IDAT (cadr cur))
               (time (caddr cur)))
          (concatenate 'list
                       (buildFCTL
                        frameNum
                        width
                        height
                        0         ; xOff
                        0         ; yOff
                        time
                        0         ; disposeOp
                        0         ; blendOp
                        )
                       (if (= framenum 0)
                           (makeChunk "IDAT" IDAT)
                           (makeChunk "fcTL" (concatenate 'list
                                                          (makeNum frameNum nil 4)
                                                          IDAT)))
                       (buildFrames rest (1+ frameNum))
                       ))))
  
  ; Returns an APNG as a byte list that has the following playtime
  ; properties:
  ;  * contains in order the frames and amount of time to display each 
  ;     frame from framedata
  ;  * loops the frames  numPlays times
  ;  * contains the number of frames as specified in numFrames
  ; numPlays = number of times to loop the animation
  ; numFrames = number of frames in the animation
  ; framedata = frame and time display information of the type 
  ;   (list (list frame displaytime)... ) where frame is a byte list 
  ;   representing a PNG's contents and displaytime is a string 
  ;   representing the amount of time in seconds to display that frame
  ;   as a rational number (i.e. 100/2997 for NTSC standard)
  (defun buildAPNG (numPlays numFrames framedata)
	(let* ((frames (preparePNGs framedata))
              (valid (validateIHDR frames nil)))
          (if (stringp valid)          ; Returns error message when needed
              valid
              (concatenate 'list
                           (list 137 80 78 71 13 10 26 10) ; PNG signature
                           (makeChunk "IHDR" (car (car frames)))
                           (buildACTL numFrames numPlays)
                           (buildFrames frames 0)
                           (makeChunk "IEND" nil)
              ))))
  
  (export IapngBuilder))
