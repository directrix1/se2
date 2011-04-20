(include-book "list-utilities" :dir :teachpacks)
(include-book "io-utilities" :dir :teachpacks)

; drop-all - Drops elements from the beginning of xs until it finds 
; one that is not in targets
;
; targets :: [object]
; xs :: [object]
; returns [object]
(defun drop-all (targets xs)
  (if (or (endp xs)
          (not (member-equal (first xs) targets)))
      xs
      (drop-all targets (rest xs))))

; drop-all - Drops elements from the end of xs until it finds one that 
; is not in targets
;
; targets :: [object]
; xs :: [object]
; returns [object]
(defun drop-all-tail (targets xs prefix)
  (if (endp xs)
      nil
      (if (member-equal (first xs) targets)
          (drop-all-tail targets (rest xs) (cons (first xs) prefix))
          (append prefix (cons (first xs) 
                               (drop-all-tail targets (rest xs) nil))))))

; trim - Removes leading and trailing whitespace
;
; chrs :: [character] - characters to trim
; returns [character]
(defun trim (chrs)
  (let ((whitespace (list #\space #\tab)))
    (drop-all-tail whitespace (drop-all whitespace chrs) nil)))

; split-on-whitespace-helper: Splits a string into the chunks that were 
; separated by whitespace
; word :: [character] - reversed characters of the current chunk being
;                       read currently.
; words :: [string] - reversed list of chunks that have been completely
;                       read so far.
; chrs :: [character] - list of all characters left to process
; whitespace :: [character] - list of characters to consider whitespace
(defun split-on-whitespace-helper (word words chrs whitespace)
  (if (endp chrs)
      (if (endp word)
          words
          (cons (reverse (coerce word 'string)) words ))
      (if (member-equal (car chrs) whitespace)
          (if (endp word)
              (split-on-whitespace-helper word
                                          words
                                          (cdr chrs)
                                          whitespace)
              (split-on-whitespace-helper nil
                                          (cons (reverse 
                                                 (coerce word 'string))
                                                words)
                                          (cdr chrs)
                                          whitespace))
          (split-on-whitespace-helper (cons (car chrs) word)
                                      words
                                      (cdr chrs)
                                      whitespace))))

; split-on-whitespace : Splits a string on the given whitespace 
; characters.
; str :: string - string to be split apart
; whitespace :: [character] - characters to treat as whitespace
;
; returns [string]. Each element will not contains any of the 
; whitespace characters.
(defun split-on-whitespace (str whitespace)
  (reverse (split-on-whitespace-helper nil nil (coerce str 'list)
                                       whitespace)))

; rat-strp-helper : Recursively tests for a string representing a 
; decimal number formatted as [0-9]*.[0.9]*
;
; chrs :: [character] - characters left to consume
; consumed-decimal :: bool - whether or not a decimal point has been hit
;
; returns bool
(defun rat-strp-helper (chrs consumed-decimal)
  (or (endp chrs)
      (cond ((member-equal (car chrs)
                           '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8  #\9))
             (rat-strp-helper (cdr chrs) consumed-decimal))
            ((equal (car chrs) #\.)
             (if consumed-decimal
                 nil
                 (rat-strp-helper (cdr chrs) t)))
            (t nil))))

; rat-strp : Tests if a string represents a decimal number formatted as
; -?[0-9]*.[0-9]*
;
; str - string to test
; returns bool
(defun rat-strp (chrs)
  (and (not (endp chrs))
       (let ((no-minus (if (equal (car chrs) #\-)
                           (cdr chrs)
                           chrs)))
         (and (not (endp no-minus)) 
              (rat-strp-helper no-minus nil)))))

; string-concat - Concatenates a list of strings into a single string.
;
; strs :: [string] - strings to concatenate
; returns string
(defun string-concat (strs)
  (if (endp strs)
      ""
      (string-append (car strs)
                     (string-concat (cdr strs)))))

; str-list->chrs-list - maps str->chrs onto a list of strings
; str-list [string]
; returns [[character]]
(defun str-list->chrs-list (str-list)
  (if (endp str-list)
      nil
      (cons (str->chrs (first str-list))
            (str-list->chrs-list (rest str-list)))))