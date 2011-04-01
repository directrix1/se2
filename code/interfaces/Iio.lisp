;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.ss" ("cce" "dracula.plt") "modular" "lang")
;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#|
   Team Steele
   Software Engineering II
   Iio
|#


(interface Iio
  
  ; Helper function for animate, calls binary-file->byte-list on a
  ; filename frame
  ; frame = filename of the png frame to open 
  (sig openFile (frame state))
  
  ; Helper function for animate, calls openFile on the framelist returning
  ; the byte-lists of each function
  ; framelist = list of frame file names
  (sig openFiles (framelist state))

  ; Writes an animated portable network graphic file to disk. 
  ; xmlfilename = (string) the name of the XML document containing
  ; information on number of frames, number of plays, PNG filenames, 
  ; and the length of time each frame is displayed.
  (sig animate (xmlfilename state))
  
  (sig configFileName (apngfilename framelist fnum))
  
  ;Helper function for suspend. Writes PNG files to disk.
  ;filelist = list of list (filename filedata).
  (sig writeFiles (filelist state))

  ; Writes a series of portable network graphic files to disk, along 
  ; with an XML document. These image files are the individual frames 
  ; of the APNG file in the parameter, and the XML document contains
  ; information on the number of frames, number of plays, PNG filenames, 
  ; and length of time each frame is displayed.
  ;apngfilename = (string) the name of the animated portable network 
  ;    graphic to be broken into individual frames.
  (sig suspend (apngfilename state))

  (sig suspendt (apengfilename state))
  
  )
