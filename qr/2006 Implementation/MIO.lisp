;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
(require "IIO.lisp")
(module MIO
  (include-book "io-utilities" :dir :teachpacks)
  (include-book "list-utilities" :dir :teachpacks)
  (include-book "binary-io-utilities" :dir :teachpacks)
  (set-state-ok t)

  #| read-text-file
filename: filename to be read
state: acl2 state for file IO
return: list of strings for each line in file read in
|#
  (defun read-text-file (filename state)
    (mv-let (str msg state)
            (file->string filename state)
            (mv str msg state)))
  
  #| read-binary-file
filename: filename to be read in
state: acl2 state for file IO
return: list of lists of binary
|#
  (defun read-binary-file (filename state)
    (mv-let (bytes msg state) 
            (binary-file->byte-list filename state)
            (mv bytes msg state)))
  
  #| write-text-file
filename: name of file to be written to
data: the data to be written (list of strings)
state: acl2 state for file IO
return: state
|#		  
  (defun write-text-file (filename data state)
    (mv-let (error state)
            (string-list->file filename data state)
            (mv error state)))
  
  #| write-binary-file
filename: the filename to be written to
data: the data to be written
state: acl2 state for file IO
|#		  
  (defun write-binary-file (filename data state)
    (mv-let (error state)
            (byte-list->binary-file filename data state)
            (mv error state)))
  
  (export IIO))