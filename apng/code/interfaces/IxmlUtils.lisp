;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")

#|
   Team Steele
   Software Engineering II
   IxmlUtils

   Functions for parsing and building the PNGA XML file.
|#

(interface IxmlUtils
  ; Delivers frame data for the files included in framelist.
  ; framelist = list of the filenames from which data will be retrieved.
  (sig parseXML (domXML))
  
  ; Devlivers the value for numplays
  (sig numplays (processedConfigDOM))

  ; Delivers the value for numframes
  (sig numframes (processedConfigDOM))

  ; Delivers the value for framelist  
  (sig framelist (processedConfigDOM))

  ; Parses XML data and delivers the number of frames, number of plays,
  ; and a list of filenames with their corresponding time length.
  ; domXML = XML data as a document object model 
  (sig grabFrames (framelist))
  
  ; Helper function for writeXML. Transforms the list of images and lengths
  ; into DOM structure.
  (sig prepFrameData (framedata))

  ; Delivers a string that is an XML document containing the information
  ; for an APNG file
  ; numPlays = the number of times the animation will play.
  ; numFrames = the total number of frames that make up the animation.
  ; framedata = a list of list (PNG filename, time). 
  (sig writeXML (numPlays numFrames framedata))

  ; Delivers a list of filenames with their time lengths for APNG 
  ; frame data.
  ; FrameData = APNG file data
  ; apngFileName = name of the apng file
  (sig writeFrames (frameData apngFileName))

;-------------------------------CONTRACTS---------------------------------;
  (con grabFrames-preserves-correct-nodes
	(implies (true-listp flist)
		(let ((children (xml-getchildren flist)))
			(= (len children) (len (grabFrames children))))))

  (con writeFrames-len-preserved
	(implies (and (true-listp flist) (stringp apngFileName))
		(= (len (writeFrames flist apngFileName)) (len (flist)))))

  (con parseXML-returns-three
	(implies (true-listp domXML)
		 (= (len (parseXML domXML)) 3)))

  (con writeXML-delivers-string
	(implies (and (and (stringp numPlays) (stringp numFrames))
		(true-lisp framedata)
			(stringp (writeXML 
				numPlays numFrames framedata)))))

  (con writeXML-reverses-parseXML
	(implies (true-listp domXML)
		(let ((parsed (parseXML domXML)))
			(equal domXML (writeXML (car parsed) (cadr parsed)
				 (caddr parsed))))))

  (con parseXML-reverses-writeXML
	(implies (and (natp frames) (natp plays) (true-listp flist))
		(let ((written (writeXML plays frames flist)))
			(and (equal plays (car written)) (equal frames (cadr written))
			     (equal flist (caddr written))))))

)