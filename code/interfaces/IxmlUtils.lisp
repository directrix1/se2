;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.ss" ("cce" "dracula.plt") "modular" "lang")
(interface IxmlUtils
  ; Delivers frame data for the files included in framelist.
  ; framelist = list of the filenames from which data will be retrieved.
  (sig parseXML (domXML))

  ; Parses XML data and delivers the number of frames, number of plays,
  ; and a list of filenames with their corresponding time length.
  ; domXML = XML data as a document object model 
  (sig getFrames (framelist))


  ; Delivers a string that is an XML document containing the information
  ; for an APNG file
  ; numPlays = the number of times the animation will play.
  ; numFrames = the total number of frames that make up the animation.
  ; framedata = a list of list (PNG filename, time). 
  (sig writeXML (numPlays numFrames framedata))

  ; Delivers a list of filenames with their time lengths for APNG 
  ; frame data.
  ; FrameData = APNG file data
  (sig writeFrames (frameData))

  (con parseXML-returns-three
	(implies (true-listp domXML)
		 (= (len (parseXML domXML)) 3)))

  (con writeXML-delivers-string
	(implies (and (and (stringp numPlays) (stringp numFrames))
		(true-lisp framedata)
			(stringp (writeXML numPlays numFrames framedata)))))
)
