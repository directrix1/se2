(include-book "file-bundle")
(include-book "arithmetic-3/top" :dir :system)

; count-from-to - Generates a sequence of natural numbers between a
; lower and upper bound.
;
; count-from-to i j = (i, i+1, i+2, ... j-2, j-1, j)
;
; from :: nat - lower bound of counting.
; to :: nat - upper bound of counting.
(defun count-from-to (from to)
  (declare (xargs :measure (acl2-count (+ 1 (- from) to ))))
  (if (and (integerp from) (integerp to) (<= from to))
      (cons from
            (count-from-to (1+ from) to))
      nil))

; generate-row - Generates a row of the Levenschtein string distance
; matrix.
;
; above-row :: [nat] - previous row in the distance matrix
; left-element :: nat - element to the left of this row
; str1 :: [character] - one string being compared
; char2 :: character - character cooresponding to this row of the
; other string
;
; returns [nat]
(defun generate-row (above-row left-element str1 char2)
  (if (consp (rest above-row))
      (cons left-element 
            (generate-row (rest above-row)
                          (if (equal (first str1) char2)
                              (first above-row)
                              (1+ (min (first above-row)
                                       (min (second above-row)
                                            left-element))))
                          (rest str1)
                          char2))
      (cons left-element nil)))


; Generates the final row of the string distance matrix
;
; returns [nat] 
(defun generate-rows (above-row n str1 str2)
  (if (endp str2) 
      (first (last above-row))
      (generate-rows (generate-row above-row n str1 (first str2))
                     (1+ n)
                     str1
                     (rest str2))))

; string-distance - Calculates the distance between two strings. 
; Distance is defined as the minimum number of removals, insertions,
; and replacements to transform one string to the other.
(defun string-distance (str1 str2)
  (generate-rows (count-from-to 0 (length str1))
                 1
                 str1
                 str2))

; make-suggestion-rec - helper function for make-suggestion
(defun make-suggestion-rec (str choices best-choice best-distance)
  (if (endp choices)
      best-choice
      (let* ((choice (first choices))
             (distance (string-distance str choice)))
        (if (or (equal best-distance nil) (< distance best-distance))
            (make-suggestion-rec str (rest choices) choice distance)
            (make-suggestion-rec str (rest choices) best-choice 
                                 best-distance)))))



; Suggests what might have been meant by 'str', given that 'choices'
; contains all the valid choices.
;
; str :: string - string to make a recommendation for
; choices :: [string] - things to consider recommending
(defun make-suggestion (str choices)
  (chrs->str (make-suggestion-rec (str->chrs str) 
                                  (str-list->chrs-list choices)
                                  nil nil)))

; Tests an object for being a list composed only of naturals
; xs :: object
; returns boolean
(defun nat-listp (xs)
  (or (null xs)
      (and (natp (first xs))
           (nat-listp (rest xs)))))

; count-from-to returns nats
(defthm count-in-nats
  (implies (and (natp low)
                (natp high))
           (nat-listp (count-from-to low high))))

; generate-row returns nats
(defthm generate-row-nats
  (implies (and (nat-listp above-row)
                (natp left-element)
                (true-listp str1)
                (= (+ 1 (len str1)) (len above-row)))
           (nat-listp (generate-row above-row left-element str1 char2))))

; The generate-row preserves row length
(defthm generate-row-length
  (implies (and (nat-listp above-row)
                (natp left-element)
                (true-listp str1)
                (= (+ 1 (len str1)) (len above-row)))
           (equal (len (generate-row above-row left-element str1 char2))  
                  (len above-row))))

; generate-rows also gives a list of nats
(defthm generate-rows-nats
  (implies (and (nat-listp above-row)
                (natp n)
                (true-listp str1)
                (= (+ 1 (len str1)) (len above-row)))
           (natp (generate-rows above-row n str1 str2)))
  )

; count-from-to is length (high - low + 1)
(defthm count-from-to-length
  (implies (and (integerp low)
                (integerp high)
                (<= low high))
           (equal (length (count-from-to low high))
                  (+ 1 high (- low)))))

; counting over the length of a list gives a certain length list
(defthm count-from-to-list-length
  (implies (true-listp xs)
           (equal (len (count-from-to 0 (len xs)))
                  (+ 1 (len xs))))
  :hints (("Goal" :do-not-induct t
                  :use (:instance count-from-to-length
                                  (low 1)
                                  (high (len xs))))))

; This distance between any two lists is non-negatives
(defthm string-distance-non-negative
  (implies (and (true-listp str1)
                (true-listp str2))
           (natp (string-distance str1 str2)))
  :hints (("Goal" :do-not-induct t)))