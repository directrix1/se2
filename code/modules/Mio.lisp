;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.ss" ("cce" "dracula.plt") "modular" "lang")
(require "../interfaces/Ibasiclex.lisp")
(require "../interfaces/IapngBuilder.lisp")
(require "../interfaces/IapngExploder.lisp")
(require "../interfaces/IxmlUtils.lisp")
(require "../interfaces/IminidomSerializer.lisp")


(module Mio  
  (include-book "list-utilities" :dir :teachpacks)

  
  ;Writes an animated portable network graphic file to disk. 
  ;xmlfilename = (string) the name of the XML document containing information on number of frames, number of plays, PNG filenames, 
  ;and the length of time each frame is displayed.
  (defun animate (xmlfilename, state)nil)

  ;Writes a series of portable network graphic files to disk, along with an XML document. These image files are the individual frames 
  ;of the APNG file in the parameter, and the XML document contains information on the number of frames, number of plays, PNG filenames, 
  ;and length of time each frame is displayed.
  ;apngfilename = (string) the name of the animated portable network graphic to be broken into individual frames.
  (defun suspend (apngfilename, state)nil)

  ;Helper function for suspend. Writes PNG files to disk.
  ;filelist = list of mv (filename filedata).
  (defun writeFiles (filelist, state)nil)
  
  (export Iio))
