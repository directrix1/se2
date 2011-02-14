#|===== Module: list-utilities =======================================
Defining
   (break-at-nth n xs)
   (break-at-set delimiters xs)
   (break-at delimiter xs)
   (take-to-set delimiters xs)
   (take-to delimiter xs)
   (drop-set delimiters xs)
   (drop-past delimiter xs)
     = (drop-thru delimiter xs)
   (drop-past-n-delimiters n delimiter xs)
     = (drop-thru-n-delimiters n delimiter xs)
   (packets-set delimiters xs)
   (packets delimiter xs)
   (tokens delimiters xs)
   (replicate n x)
   (pad-left width pad xs)
   (chrs->str character-list)
   (str->chrs string)
   (words str)
====================================================================|#

#|===== Usage Notes ==================================================
 To make definitions in this module available, put this command
 in the importing module:
   (include-book "list-utilities" :dir :teachpacks)
====================================================================|#

;===== Environment setup ============================================
  (in-package "ACL2")
  (include-book "arithmetic-3/extra/top-ext" :dir :system)
  (include-book "arithmetic-3/floor-mod/floor-mod" :dir :system)
;=====================================================================

#|====== Function: (break-at-nth n xs) ===============================
  Returns list with two elements
    (1) the first n elements of xs
    (2) xs without its first n elements

  Pre : (and (integerp n)
             (>= n 0)
             (true-listp xs))
  Post: (and (= (min n (length xs))
                (length (car (break-at-nth n xs))))
             (equal (append (car  (break-at-nth n xs))
                            (cadr (break-at-nth n xs)))
                    xs))
====================================================================|#
(defun break-at-nth (n xs)
  (if (or (not (integerp n))
          (<= n 0)
          (not (consp xs)))
      (list '() xs)
      (let* ((first-thing (car xs))
             (break-of-rest (break-at-nth (- n 1) (cdr xs)))
             (prefix (car break-of-rest))
             (suffix (cadr break-of-rest)))
          (list (cons first-thing prefix) suffix))))

#|====== Function: (break-at-set delimiters xs) ======================
  Returns list with two elements
    (1) the portion of xs that precedes its first element that is a
        member of the list delimiters
    (2) the portion of xs beginning with its first element that is a
        member of the list delimiters

  Precondition:
  (and (true-listp delimiters)
       (true-listp xs))
  Postcondition:
  (and (equal (append (car (break-at-set ds xs))
                      (cadr (break-at-set ds xs)))
              xs)
        (implies (consp (cadr (break-at-set ds xs)))
                 (member-equal (caadr (break-at-set ds xs)) ds)))
        (implies (member-equal d ds)
                 (not (member-equal d (car (break-at-set ds xs))))))
====================================================================|#
(defun break-at-set (delimiters xs)
  (if (or (not (consp xs))
          (member-equal (car xs) delimiters))
      (list '() xs)
      (let* ((first-thing (car xs))
             (break-of-rest (break-at-set delimiters (cdr xs)))
             (prefix (car break-of-rest))
             (suffix (cadr break-of-rest)))
          (list (cons first-thing prefix) suffix))))

#|====== Function: (break-at x xs) ===================================
  Returns list with two elements
    (1) the portion of xs that precedes the first element equal to x
    (2) the portion of xs beginning with the first element equal to x

  Preconditon:
  (true-listp xs)
  Postcondition
  (and (equal (append (car (break-at x xs)) (cadr (break-at x xs)))
               xs)
       (implies (consp (cadr (break-at d xs)))
                (member-equal (caadr (break-at-set d xs)) ds)))
       (implies (member-equal d ds)
                (not (member-equal d (car (break-at d xs))))))
====================================================================|#
(defun break-at (delimiter xs)
   (break-at-set (list delimiter) xs))

#|====== Function: (take-to-set delimiters xs) =======================
  Returns the part of xs that precedes its first element that 
  is a memberof the list delimiters

  Precondition:
  (and (true-listp delimiters)
       (true-listp xs))
  Postcondition:
  (and (true-listp (take-to-set delimiters xs))
       (implies (member-equal d delimiters)
                (not (member-equal d (take-to-set delimiters xs))))
       (implies (and (equal (append (take-to-set delimiters xs) ys)
                            xs)
                     (consp ys))
                (member-equal (car ys) delimiters)))
====================================================================|#
(defun take-to-set (delimiters xs)
   (car (break-at-set delimiters xs)))

#|====== Function: (take-to x xs) ====================================
  Returns the part of xs that precedes the first element equal to x

  Pre : (true-listp xs)
  Post: (and (true-listp (take-to x xs))
             (not (member-equal x (take-to x xs)))
             (implies (member-equal x xs)
                      (equal (append (take-to x xs)
                                     (list x)
                                     (drop-past x xs))))
             (implies (not (member-equal x xs))
                      (equal (take-to x xs) xs)))
====================================================================|#
(defun take-to (x xs)
   (take-to-set (list x) xs))

#|===== Function: (drop-set delimiters xs) ===========================
  Returns the part of xs that follows the maximal contiguous block of
  delimiters beginning at the first element of xs

  Pre:  (true-listp xs)
  Post: (and (true-listp (drop-set delimiters xs))
             (implies (consp (drop-set delimiters xs))
                      (not (member (car (drop-set delimiters xs))
                                   delimiters)))
             (implies (and (equal (append ds (drop-set delimiters xs))
                                  xs)
                           (member d ds))
                      (member d delimiters)))
====================================================================|#
(defun drop-set (delimiters xs)
  (if (and (consp xs)
           (member-equal (car xs) delimiters))
      (drop-set delimiters (cdr xs))
      xs))

#|===== Function: (drop-past delimiter xs) ===========================
  Returns the part of xs that follows the first element equal to x

  Pre:  (true-listp xs)
  Post: (and (true-listp (drop-past delimiter xs))
             (implies (member-equal x xs)
                      (equal (append (take-to delimiter xs)
                                     (list x)
                                     (drop-past delimiter xs))))
             (implies (not (member-equal delimiter xs))
                      (equal (drop-past delimiter xs) nil)))
====================================================================|#
(defun drop-past (delimiter xs)
  (cdr (member-equal delimiter xs)))
(defun drop-thru (delimiter xs) ; Butch's synonym
  (drop-past delimiter xs))

#|====== Function: (drop-past-n-delimiters n delimiter xs) ===========

  Returns the part of xs that follows the first n occurances of x

  Pre : (and (integerp n) (>= n 0) (true-listp xs))
  Post: (true-listp (drop-past-n-delimiters n d xs)) ;and other things
====================================================================|#
(defun drop-past-n-delimiters (n delimiter xs)
   (if (or (not (integerp n))
           (<= n 0)
           (not (consp xs)))
       xs
       (drop-past-n-delimiters (- n 1)
                               delimiter 
                               (drop-past delimiter xs))))
(defun drop-thru-n-delimiters (n delimiter xs) ; Butch's synonym
   (drop-past-n-delimiters n delimiter xs))

#|====== Function: (packets-set delimiters xs) =======================
  Parcels a list into a list of lists
  Contiguous sequences from a specified set of delimiters
  marks separations between packets

  Pre : (and (true-listp delimiters) (true-listp xs))
  Post: (true-list-listp (packets-set ds xs))  ; among other things
====================================================================|#

#|
(defthm break-at-set-cadr-begins-with-delimiter
  (implies (consp (cadr (break-at-set delimiters xs)))
           (member-equal (caadr (break-at-set delimiters xs)) 
                         delimiters)))

(defthm car-cdr-cons-identity
   (implies (consp xs)
            (equal (cons (car xs) (cdr xs))
                   xs)))

(defthm cdr-chops-non-empty-lists
  (implies (and (true-listp xs) (consp xs))
           (< (length (cdr xs)) (length xs))))

(defthm drop-set-chops-if-list-starts-with-delimiter
  (implies (and (true-listp xs)
                (true-listp delimiters)
                (consp xs)
                (member-equal (car xs) delimiters))
           (< (length (drop-set delimiters xs))
              (length xs))))
|#
(defun packets-set (delimiters xs)
  (if (not (consp xs))
      '(nil)
      (let* ((pkt-split (break-at-set delimiters xs))
             (first-pkt (car pkt-split))
             (after-1st (cadr pkt-split))
             (othr-pkts (if (not (consp after-1st))
                            nil
                            (packets-set delimiters
                                         (cdr after-1st)))))
        (cons first-pkt othr-pkts))))

#|====== Function: (packets delimiter xs) ============================
  Parcels a list into a list of lists
  A specified delimiter marks separations between packets in the given
   list

  Precondition:
  (true-listp xs)
  Postcondition:
  (and (true-list-listp (packets d xs))
       (implies (and (integerp n) (>= n 0))
                (equal (nth n (packets d xs))
                       (take-to d (drop-past-n-delimiters n d xs)))))
====================================================================|#
(defun packets (delimiter xs)
  (packets-set (list delimiter) xs))

#|====== Function: (tokens delimiters xs) ============================
  Parcels a list into a list of tokens
  Tokens are the sublists residing between maximally contiguous
  sequences of delimiters

  Pre : (and (true-listp delimiters) (true-listp xs))
  Post: (true-list-listp (tokens ds xs))
        and a bunch of other stuff
====================================================================|#
(defun tokens (delimiters xs)
  (remove nil (packets-set delimiters xs)))

#|===== Function: (rep-append n x xs) ===============================

  Appends xs to a list consisting of n copies of x

  Pre : (and (integerp n)
             (>= n 0)
             (true-listp xs))
  Post: (and (implies (member-equal y (take n (rep-append n x)))
                      (equal y x))
             (equal (length (rep-append n x xs))
                    (+ (length xs) n))
             (equal (nthcdr n (rep-append n x xs))
                    xs))
====================================================================|#
(defun rep-append (n x xs)
  (if (or (not (integerp n))
          (<= n 0))
      xs
      (rep-append (- n 1) x (cons x xs))))

#|===== Function: (replicate n x) ====================================

  Delivers a list consisting of n copies of x

  Pre : (and (integerp n)
             (>= n 0))
  Post: (and (implies (member-equal y (replicate n x))
                      (equal y x))
             (equal (length (replicate n x)) n))
====================================================================|#
(defun replicate (n x)
  (rep-append n x nil))

#|===== Function: (pad-left w p xs) ==================================

  Pads appends xs to copies of p to make the resulting list have w 
   elements
  Note: Delivers xs, as is, if xs has w or more elements

  Precondition:
  (and (integerp w)
       (>= w 0)
       (listp xs))
  Postcondition:
  (and (equal (length (pad-left w p xs))
              (max w (length xs)))
       (implies (member-equal x (take (max 0 (- w (length xs))) xs))
                (equal x p)))
====================================================================|#
(defun pad-left (w p xs)
  (append (replicate (max 0 (- w (length xs))) p)
          xs))

#|====== Function: (chrs->str chrs) ==================================
  Converts list of characters to string

  Pre : (character-listp chrs)
  Post: (stringp (chrs->str chrs))              
====================================================================|#
(defun chrs->str (chrs)
  (coerce chrs 'string))

#|====== Function: (str->chrs str) ===================================
  Converts string to list of characters

  Pre : (stringp str)
  Post: (character-listp (str->chrs str))
====================================================================|#
(defun str->chrs (str)
  (coerce str 'list))

#|====== Function: (words str) =======================================
  Parcels a string into a list of words
  Words are the sublists residing between maximally contiguous
  spans of whitespace

  Pre : (stringp str)
  Post: (string-listp (words str))
        and a bunch of other stuff
====================================================================|#
(defun chrs->str-all (list-of-lists-of-chrs)
  (if (consp list-of-lists-of-chrs)
      (cons (chrs->str (car list-of-lists-of-chrs))
            (chrs->str-all (cdr list-of-lists-of-chrs)))
      nil))
(defun words (str)
  (let* ((whitespace (list (code-char 32) (code-char 10)
                           (code-char 9) (code-char 11) 
                           (code-char 12) (code-char 13)
                           (code-char 27))))
     (chrs->str-all (remove nil 
                            (tokens whitespace (str->chrs str))))))


