;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#| Team Steele
   Software Engineering II
   IapngBuilder

   Interface for building APNG files from PNG IHDR and IDAT chunks
|#

(interface IapngBuilder
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
  ;   representing a PNG's contents and displaytime is a byte list 
  ;   representing the amount of time in seconds to display that frame
  ;   as a rational number (i.e. 100/2997 for NTSC standard)
  (sig buildAPNG (numPlays numFrames framedata))

  ; This function formats the raw PNG file data into more conveniently 
  ; utilized chunks, and returns a list of (list IHDR IDAT displaytime)
  ; where IHDR is the IHDR chunk IDAT is the IDAT chunk and time is the
  ; displaytime is the corresponding value from the framedata parameter.
  ; framedata = frame and time display information of the type 
  ;   (list (list frame displaytime)... ) where frame is a byte list
  ;   representing a PNG's contents and displaytime is a byte list
  ;   representing the amount of time in seconds to display that frame as
  ;   a rational number (i.e. 100/2997 for NTSC standard)
  (sig preparePNGs (framedata))

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
  (sig validateIHDR (prepdPNGs baseIHDR))

  ; Convert the prepdPNGs parameter into a byte list representation of the
  ; APNG frames with their associated fcTL, IDAT and fdAT chunks using 
  ; prepdPNGs as a source for the framedata. This does not include the 
  ; file signature, IHDR or acTL chunk.
  ; prepdPNGs = prepared list of PNGs in the form described as the output
  ;    of preparePNGs
  ; frameNum = if 0 then the image data of the first PNG of the list will
  ;    be output as an IDAT chunk all other frames are fdAT chunks
  (sig buildFrames (prepdPNGs frameNum))

  ; Returns a byte list representing the acTL chunk described by the
  ; parameters.
  ; numPlays = number of times the animation is intended to be played
  ; numFrames = the number of frames in the animation
  (sig buildACTL (numPlays numFrames))

  ; Returns a byte list representing the fcTL chunk described by the
  ; parameters.
  ; sequenceNum = the sequence number in the animation
  ; width = width of the frame
  ; height = height of the frame
  ; xOffset = xOffset of the frame
  ; yOffset = yOffset of the frame
  ; delayTime = frame delay byte list in the form of a rational number
  ;    (i.e. 100/2997) in seconds
  ; disposeOp = after display do 0 = nothing, 1 = transparent black,
  ;     2 = revert to previous frame
  ; blendOp = 0 = overwrite all color components including alpha,
  ;     1 = blend over
  (sig buildFCTL (sequenceNum width height xOffset yOffset delayTime
                  disposeOp blendOp))

  ;;;;;;;;;;;;;;;;;;;;;;;;; Contracts
;  (con buildFrames-returns-byte-list
;	(implies (stringp prepd)
;		(strip (buildFrames prepd 0))))
;
;  (con buildACTL-returns-valid-chunk
;	(implies (and (stringp numPlays)
;		      (stringp numFrames))
;		 (equal (buildACTL numPlays numFrames) (makeChunk "acTL"
;                        (ascii->byte (concatenate numPlays numFrames))))))
;
;  (con buildFCTL-returns-valid-chunk
;       (implies (and (stringp seqNum) (stringp width) (stringp height)
;		     (stringp xOff) (stringp yOff) (stringp delay)
;		     (stringp dispOp) (stringp blendOp)
;		(equal (buildFCTL seqNum width height xOff yOff delay 
;				  dispOp blendOp)
;		       (makeChunk "fcTL" (ascii->byte (concatenate seqNum
;			width height xOff yOff delay dispOp blendOp)))))))


)
