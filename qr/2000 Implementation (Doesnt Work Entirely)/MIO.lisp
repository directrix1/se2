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
  
  (defun read-text-file (filename state)
    (mv-let (str msg state)
            (file->string filename state)
            (mv str msg state)))
  
  (defun read-bitmap-file (filename state)
    (mv-let (bytes msg state) 
            (binary-file->byte-list filename state)
            (mv bytes msg state)))
  
  (defun write-text-file (filename data state)
    (mv-let (error state)
            (string-list->file filename data state)
            (mv error state)))
  
  (defun write-bitmap-file (filename data state)
    (mv-let (error state)
            (byte-list->binary-file filename data state)
            (mv error state)))
  
  (export IIO)
)
