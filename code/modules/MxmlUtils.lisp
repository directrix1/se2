;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#| Team Steele
   Software Engineering II
   TxmlUtils

   Functions to assist with parsing and constructing XML files.
|#


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
     (if (null domXML) nil
	(let* ((pngaxml domXML)
	       (numPlays (xml-getattribute pngaxml "plays"))
	       (numFrames (xml-getattribute pngaxml"frames"))
	       (frames (getFrames (xml-getchildren pngaxml))))
	      (list numPlays numFrames frames))))

   (defun numplays (processedConfigDOM)
     (first processedConfigDOM))

   (defun numframes (processedConfigDOM)
     (second processedConfigDOM))

   (defun framelist (processedConfigDOM)
     (third processedConfigDOM))
    
   ; Helper function for writeXML. Transforms the list of images and lengths
   ; into DOM structure.
   (defun prepFrameData (framedata)
     (if (endp framedata) nil
          (append(list (list "image" (list (list "src" (car (car framedata)))
                             (list "length" (cadr (car framedata))))nil))
               (prepFrameData(cdr framedata)))))
  
  
   ;Delivers a string that is an XML document containing the information
   ;for an APNG file
   ;numPlays = the number of times the animation will play.
   ;numFrames = the total number of frames that make up the animation.
   ;framedata = a list of list (PNG filename, time). 
   (defun writeXML (numPlays numFrames framedata)
     (if (not numPlays) "Invalid data"
         (if (not numFrames) "Invalid data"
             (if (not framedata) "Invalid data"
     (xml-serialize-dom (list "pnga" (list (list "frames" numFrames)
                                           (list "plays" numPlays))
                                     (prepFrameData framedata)))))))

   ;Helper function for writeFrames
   ;FrameData = APNG file data in the form: ((framedata1 time_for_frame1)
   ;			                     (framedata2 time_for_frame2)...
   ;apngFileName = name of the apng file
   ;Delivers a list of generic file names for framedata along with the
   ;time lengths. The order of the files are reversed.
   (defun nameFrames (frameData apngFileName)
     (if (endp frameData) nil (if (null apngFileName) nil
         (cons (list (string-append apngFileName (string-append
                                   (string (car
                                    (explode-nonnegative-integer
                                     (len frameData) 10 nil))) ".png"))
               (cadr (car (reverse frameData))))
               (nameFrames (reverse (cdr (reverse frameData))) apngFileName)))))
  
   ;Delivers a list of filenames with their time lengths for APNG 
   ;frame data.
   ;frameData = APNG file data in the form: ((framedata1 time_for_frame1)
   ;			                     (framedata2 time_for_frame2)...
   ;apngFileName = name of the apng file
  (defun writeFrames (frameData apngFileName)
    (reverse (nameFrames frameData apngFileName)))
  
  
(export IxmlUtils))

