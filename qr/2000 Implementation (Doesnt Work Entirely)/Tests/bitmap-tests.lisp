(include-book "testing" :dir :teachpacks)
(include-book "doublecheck" :dir :teachpacks)
(include-book "bitmap-list-functions")

;list get checks for
;(true-listp xs)
;(posp n)
;(<= (+ n o) (len xs))
;
;so I am excluding those cases

;arguments that don't make sense
(check-expect (list-get 0 0 nil '()) nil)
(check-expect (list-get 0 0 nil '(1 2 3)) nil)
(check-expect (list-get 4 0 nil '()) nil)
(check-expect (list-get 4 2 nil '()) nil)
(check-expect (list-get 0 0 t '(1 2 3)) nil)
(check-expect (list-get 4 0 t '()) nil)
(check-expect (list-get 4 2 t '()) nil)
(check-expect (list-get 2 10 nil '(1 2 3 4 5)) '())
(check-expect (list-get 2 10 t '(1 2 3 4 5)) '())
(check-expect (list-get 20 1 nil '(1 2 3 4 5)) '())
(check-expect (list-get 20 1 t '(1 2 3 4 5)) '())

;arguments that do make sense
(check-expect (list-get 1 1 nil '(1 2 3 4 5)) '(2))
(check-expect (list-get 1 1 t '(1 2 3 4 5)) '((2) (3 4 5)))
(check-expect (list-get 2 1 nil '(1 2 3 4 5)) '(2 3))
(check-expect (list-get 2 1 t '(1 2 3 4 5)) '((2 3) (4 5)))

;(defun partition (s l)
;  (if (endp l)
;      nil
;      (cons (take s l) (partition s (nthcdr s l)))))

;arguments that don't make sense
(check-expect (partition 0 '()) nil)
(check-expect (partition 0 '(1 2 3)) nil)
(check-expect (partition 2 '()) nil)

;arguments that do make sense
(check-expect (partition 1 '(1 2 3)) '((1) (2) (3)))
(check-expect (partition 2 '(1 2 3)) '((1 2) (3 nil)))
(check-expect (partition 3 '(1 2 3 4 5)) '((1 2 3) (4 5 nil)))


;(defun every-nth (n l)
;  (if (< (len l) n)
;      nil
;      (cons (nth (- n 1) l) (every-nth n (nthcdr n l)))))

;arguments that don't make sense
(check-expect (every-nth 0 '()) nil)
(check-expect (every-nth 3 '()) nil)
(check-expect (every-nth 0 '(1 2 3 4 5 6)) nil)

(check-expect (every-nth 1 '(1 2 3 4 5 6)) '(1 2 3 4 5 6))
(check-expect (every-nth 2 '(1 2 3 4 5 6)) '(2 4 6))