;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")

(require "../interfaces/Ibasiclex.lisp")
(require "../interfaces/IxmlUtils.lisp")
(require "../interfaces/IminidomSerializer.lisp")
(require "../interfaces/IminidomParser.lisp")


(module MxmlUtils  
  
  (import Ibasiclex)
  (import IminidomParser)
  (import IminidomSerializer)
  (include-book "list-utilities" :dir :teachpacks)  

   ;Delivers a list, where each item corresponds to the metadata for each
   ;image element specified in the xml config.  An item consists of two
   ;elements: the file name and the length of time the frame should be
   ;displayed.
   ;framelist = list of minidom "image" nodes
   (defun getFrames (framelist)
     (if (null framelist) nil
	(let* ((nextFrame (car framelist))
	       (src (xml-getattribute nextFrame "src"))
	       (timelen (xml-getattribute nextFrame "length")))
	   (cons (list src timelen) (getFrames (cdr framelist))))))

   ;Parses XML data and delivers the number of frames, number of plays,
   ;and a list of filenames with their corresponding time length.
   ;domXML = XML data as a document object model 
   (defun parseXML (domXML)
	(let* ((pngaxml (xml-getnode domXML "pnga"))
	       (numPlays (xml-getattribute pngaxml "plays"))
	       (numFrames (xml-getattribute pngaxml"frames"))
	       (frames (getFrames (xml-bfsfindnodes pngaxml "image"))))
	      (list numPlays numFrames frames)))

   (defun numplays (processedConfigDOM)
     (first processedConfigDOM))

   (defun numframes (processedConfigDOM)
     (second processedConfigDOM))

   (defun framelist (processedConfigDOM)
     (third processedConfigDOM))

   ;Delivers a string that is an XML document containing the information
   ;for an APNG file
   ;numPlays = the number of times the animation will play.
   ;numFrames = the total number of frames that make up the animation.
   ;framedata = a list of list (PNG filename, time). 
   (defun writeXML (numPlays numFrames framedata)nil)

   ;Delivers a list of filenames with their time lengths for APNG 
   ;frame data.
   ;FrameData = APNG file data

   (defun writeFrames (frameData) nil)
  
  
(export IxmlUtils))

