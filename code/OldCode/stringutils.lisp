#| Edward Flick
   Software Engineering I
   stringutils

   This file provides some helper functions for handling strings.
|#

(in-package "ACL2")

; stringlist-append (stringlist)
;  stringlist = list of strings
; returns: the appended composite of all strings in stringlist
(defun stringlist-append (stringlist)
  (if stringlist
      (string-append (car stringlist) (stringlist-append (cdr stringlist)))
      ""))

; stringlist? (stringlist)
; returns: true if stringlist is a list of strings
(defun stringlist? (stringlist)
  (and
   (true-listp stringlist)
   (or
    (null stringlist)
    (and
     (stringp (car stringlist))
     (stringlist? (cdr stringlist))))))