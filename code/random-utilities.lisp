(in-package "ACL2")
(include-book "doublecheck" :dir :teachpacks)
(include-book "list-utilities" :dir :teachpacks)

; generate a random permutation of the n-element prefix of xs
; n  = natural number
; xs = (x1 x2 x3 ... xm), m >= n
; (random-permutation-of-first-n n xs) = permutation of (x1 x2 ... xm) 
(defrandom random-permutation-of-first-n (n xs)
  (if (zp n)
      nil
      (let* ((n-1 (1- n))
             (k   (random-between 0 n-1))
             (br  (break-at-nth k xs))
             (bf  (car br))
             (at  (cadr br))
             (x   (car at))
             (af  (cdr at)))
        (cons x (random-permutation-of-first-n n-1 (append bf af))))))
(defrandom random-permutation-of (xs)
  (random-permutation-of-first-n (len xs) xs))

(defrandom random-draw (balls)
  (nth (random-between 0 (1- (len balls))) balls))
