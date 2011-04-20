(include-book "utilities")
(include-book "testing" :dir :teachpacks)

;empty list is nil
(check-expect (put-nth 1 2 '()) nil)
;n out of list bounds does nothing
(check-expect (put-nth 5 1 '(2 3)) '(2 3))
;regular cases
(check-expect (put-nth 0 3 '(2 4 6)) '(3 4 6))
(check-expect (put-nth 2 3 '(2 4 6)) '(2 4 3))
(check-expect (put-nth 1 3 '(2 4 6)) '(2 3 6))

;n = 0 -> empty list (value repeated 0 times)
(check-expect (createList 0 #\a) '())
;regular cases
(check-expect (createList 1 #\a) '(#\a))
(check-expect (createList 3 '(1 2 3)) '((1 2 3) (1 2 3) (1 2 3)))

; no real edge cases. always returns a list
(check-expect (true-listp (getBytes "4")) t)
;some randomly chosen characters
(check-expect (getBytes "`") '(96))
(check-expect (getBytes "F") '(70))
(check-expect (getBytes "+") '(43))

;always returns 32 bit binary number
(check-expect (len (int->bin 4294967296)) 32)
(check-expect (len (int->bin -4294967296)) 32)
;even on overflow cases
(check-expect (len (int->bin 4294967297)) 32)
(check-expect (len (int->bin -4294967297)) 32)
;regular tests
(check-expect (int->bin -4294997) '(1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 0 0 1 1 1 0 1 1 0 1 0 1 0 1 0 1 1))

(defconst *-4294997*
  '(1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 0 0 1 1 1 0 1 1 0 1 0 1 0 1 0 1 1))

;edge cases
(check-expect (bin->int (createList 32 0)) 0)
(check-expect (bin->int (createList 32 1)) -1)
;inversion of test for int->bin from above
(check-expect (bin->int *-4294997*) -4294997)

;more inversion tests
(check-expect (bin->int (int->bin 0)) 0)
(check-expect (bin->int (int->bin -1)) -1)
(check-expect (bin->int (int->bin 12345678)) 12345678)
(check-expect (int->bin (bin->int (createList 32 0))) (createList 32 0))
(check-expect (int->bin (bin->int (createList 32 1))) (createList 32 1))
(check-expect (int->bin (bin->int *-4294997*)) *-4294997*)

;edge cases
(check-expect (normalizei 0) 0)
(check-expect (normalizei 33) 1)
(check-expect (normalizei -1) 31)
;regular
(check-expect (normalizei 65) 1)
(check-expect (normalizei 1032) 8)
(check-expect (normalizei -65) 31)
(check-expect (normalizei -1032) 24)

;edge cases
(check-expect (<< 0 0) 0)
(check-expect (<< 0 3754) 0)
(check-expect (<< 53 0) 53)
(check-expect (<< 1 31) -2147483648)
;regular cases
(check-expect (<< 1 1) 2)
(check-expect (<< 2 1) 4)
(check-expect (<< 4 1) 8)

;edge cases
(check-expect (>> 0 0) 0)
(check-expect (>> 0 3754) 0)
(check-expect (>> 53 0) 53)
(check-expect (>> 1 32) 0)
;regular cases
(check-expect (>> 1 1) 0)
(check-expect (>> 2 1) 1)
(check-expect (>> 4 1) 2)

#|
Theorems that are commented don't work yet =(
|#

;check each conditional case - put-nth works on empty list,
; put-nth works on n = 0, put-nth works on both
; increases length if ! n = 0 or endp l			
(defthm put-nth-nil-on-empty-list
  (implies (endp l)
           (equal (put-nth n v l) nil)))
;(defthm put-nth-equals-v+l-if-n-zero
;  (implies (zp n)
;           (equal (put-nth n v l)
;                  (cons v (cdr l)))))
;(defthm put-nth-inserts-v-at-n-in-l
;  (implies (and (integerp n) (<= 0 n) (true-listp l))
;           (equal (nth n (put-nth n v l)) v)))
(defthm put-nth-increases-len-if-l-not-nil
  (implies (consp l)
           (>= (len (put-nth n v l))
              (len l))))

; length of result = n
; each item in result = v
;(defthm createListLoop-len-equals-n
;  (implies (posp n)
;           (= (len (createListLoop n v l)) n)))
;(defthm createListLoop-makes-list-of-v
;           (equal (subsetp (createListLoop n v l) (list v)) t))


  ;returns list of the same length as content
;(defthm getBytes-preserves-length
;  (implies (> (len content) 0)
;           (equal (len (getBytes content)) (len content))))

; is the inverse of bin->int
; is the inverse of int->bin, always less than some MAX VAL

(defthm binToint-on-zero-is-zero
  (implies (equal num 0)
           (equal (binToint from to num) 0)))

;(defthm intTobin-on-zero-is-zero
;  (implies (equal num 0)
;           (equal (intTobin from to num) 0)))

;(defthm binToint-inverts-intTobin
;  (implies (and (< num 2147483647) (> num -2147483648))
;  (equal (binToint from to (intTobin from to num)) num)))

;(defthm intTobin-inverts-binToint
;  (implies (and (< num 2147483647) (> num -2147483648))
;  (equal (binToint from to (intTobin from to num)) num)))


; abs(result) < 32 for all i
(defthm normalizei-abs-lte-32
  (<= (abs (normalizei i)) 32))

;never exceeds some MAX VAL
;(defthm <<-wraps-at-2147483647
;  (implies (and (equal num 1) (integerp i))
;  (<= (<< num i) 2147483647)))

;never inceeds some MIN VAL
;(defthm >>-always-positive
;  (implies (and (integerp num) (integerp i))
;           (>= (>> num i) 0)))

;(defthm >>>-adds-i-zeros
;  (equal (len (>>> num i))
;         (+ (len num) i)))