;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#|
   Team Steele
   Software Engineering II
   Iio
|#
(in-package "ACL2")

(require "../interfaces/Iio.lisp")
(require "../interfaces/Ibasiclex.lisp")
(require "../interfaces/IapngBuilder.lisp")
(require "../interfaces/IapngExploder.lisp")
(require "../interfaces/IxmlUtils.lisp")
(require "../interfaces/IminidomSerializer.lisp")

(require "../interfaces/IpngUtils.lisp")

(module Mio
  (include-book "list-utilities" :dir :teachpacks)
  (include-book "io-utilities" :dir :teachpacks)
  (include-book "binary-io-utilities" :dir :teachpacks)

  (import Ibasiclex)
  (import IapngBuilder)
  (import IapngExploder)
  (import IxmlUtils)
  (import IminidomSerializer)
  (import IpngUtils)

  ; Helper function for animate, calls binary-file->byte-list on a
  ; filename frame
  ; frame = filename of the png frame to open 
  (defun openFile (frame state)
    (mv-let (bytes status state)
      (binary-file->byte-list frame state)
      (if status (mv status state) bytes)))

  ; Helper function for animate, calls openFile on the framelist returning
  ; the byte-lists of each function
  ; framelist = list of frame file names
  (defun openFiles (framelist state)
    (if (endp framelist) nil
    (let* ((nextFrame (caar framelist))
           (nextLen (car (cdar framelist))))
      (cons (list (openFile nextFrame state) nextLen) 
      (openFiles (cdr framelist) state)))))
         
  ; Writes an animated portable network graphic file to disk. 
  ; xmlfilename = (string) the name of the XML document containing
  ; information on number of frames, number of plays, PNG filenames, 
  ; and the length of time each frame is displayed.
  (defun animate (xmlfilename state)
    (mv-let (xmlcontents status state)
      (file->string (string-append xmlfilename ".xml") state)
      (if status
        (mv status state)
        (let* ((xmlraw (xml-readnode xmlcontents))
               (xmlprocessed (parseXML xmlraw)) 
               (numplays (numplays xmlprocessed))
               (numframes (numframes xmlprocessed))
               (framelist (framelist xmlprocessed))
               (framedata (openFiles framelist state))
	       (rawapng (buildAPNG numplays numframes framedata)))
          (if (stringp rawapng)
              (mv rawapng state)	
              (mv-let (status-close state)
                      (byte-list->binary-file (string-append xmlfilename
                                                             ".apng") rawapng state)
                      (if status-close 
                          (mv status-close state)
                          (mv (concatenate 'string
                                           "Read png files from ["
                                           xmlfilename ".xml] and wrote ["
                                           xmlfilename ".apng]")
                              state))))))))


  (defun configFileName (apngfilename framelist fnum)
	(if (null framelist)
		nil
		(let* ((next (car framelist))
			   (pngdat (car next)))
			(cons (list (append (append apngfilename 
                                                    (rat->str fnum 0))
					   ".png") pngdat)
					   (configFileName 
							apngfilename 
                                                        (cdr framelist) 
                                                        (+ fnum 1))))))
  
   ;Helper function for writeFrames
   ;FrameData = APNG file data in the form: ((framedata1 time_for_frame1)
   ;			                     (framedata2 time_for_frame2)...
   ;apngFileName = name of the apng file
   ;Delivers a list of generic file names for framedata along with the
   ;time lengths. The order of the files are reversed.
   (defun nameTheseFrames (frameData apngFileName)
     (if (endp frameData) nil (if (null apngFileName) nil
         (cons (list (string-append apngFileName (string-append
                                   (string (car
                                    (explode-nonnegative-integer
                                     (len frameData) 10 nil))) ".png"))
               (car (reverse frameData)))
               (nameTheseFrames (reverse (cdr (reverse frameData))) apngFileName)))))

  ;Helper function for suspend. Writes PNG files to disk.
  ;filelist = list of list (filename filedata).
  (defun writeFiles (filelist state)
    (if (endp filelist) (mv "OK" state)
        (let ((filename (caar filelist))
              (filedata (cadar filelist)))
          (mv-let (error state)
            (if (equal filename "Config.xml")
                (string-list->file filename (list filedata) state)
                (byte-list->binary-file filename filedata state))
            (if error
                (mv error state)
                (writeFiles (cdr filelist) state))))))
  
  
  (defun stripTime (data)
    (if (null data)
        nil
        (cons (caar data) (stripTime (cdr data)))))

 ;Writes a series of portable network graphic files to disk, 
 ;along with an XML document. These image files are the individual frames 
 ;of the APNG file in the parameter, and the XML document contains 
 ;information on the number of frames, number of plays, PNG filenames, 
 ;and length of time each frame is displayed.
 ;apngfilename = (string) the name of the animated portable network graphic 
 ;to be broken into individual frames.
 (defun suspend (apngfilename state)
	(mv-let (apngcontents status state)
	  (binary-file->byte-list (string-append apngfilename ".apng") state)
	  (if status
		  (mv status state)
		  (let* ((exploded (explodeAPNG (nthcdr 8 apngcontents)))
				 (frames (first exploded))
				 (plays (second exploded))
				 (framedat (third exploded))
				 (xml (writeXML plays frames 
                                              (writeFrames 
                                                framedat apngfilename)))
                                 (data-minus-time (stripTime framedat))
				 (frames-with-names 
					(cons (list "Config.xml" xml) 
                                            (nameTheseFrames
                                              data-minus-time apngfilename))))
			(writeFiles frames-with-names state)))))   

  
  (defun suspendt (apngfilename state)
	(mv-let (apngcontents status state)
	  (binary-file->byte-list (string-append apngfilename ".apng") state)
          (if status
              (mv status state)
              (explodeAPNG (nthcdr 8 apngcontents)))))
;              (let* ((exploded (explodeAPNG (nthcdr 8 apngcontents)));
;			 (frames (first exploded))
;			 (plays (second exploded))
;                        (framedat (third exploded)))
;                (frames plays framedat)))))
  
  (export Iio))
