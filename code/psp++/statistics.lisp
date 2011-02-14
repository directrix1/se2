; (sum xs) -- Calculates the sum of the numbers in true-list xs
(defun sum (xs)
  (if (and (cdr xs) (true-listp xs))
      (+ (car xs) (sum (cdr xs)))
      (car xs)))

; (sqr x) -- Calculates the square of a single numeric value x.
(defun sqr (x)
  (* x x))

; (listMinusConst xs c) -- Subtracts a constant value c from each 
;    element in a true-list of numbers.
(defun listMinusConst (xs c)
  (if (and (cdr xs) (true-listp xs))
      (cons (- (car xs) c) (listMinusConst (cdr xs) c))
      (cons (- (car xs) c) nil)))

; (listSqr xs) -- Creates a true-list containing the square of each 
;    element in a true-list of numbers.
(defun listSqr (xs)
  (if (and (cdr xs) (true-listp xs))
      (cons (sqr (car xs)) (listSqr (cdr xs)))
      (cons (sqr (car xs)) nil)))

; (listXlist xs ys) -- Creates a true-list containing the products of
;    corresponding numbers in a pair of true-lists.
(defun listXlist (xs ys)
  (if (and (cdr xs) (true-listp xs) (true-listp ys))
      (cons (* (car xs) (car ys)) (listXlist (cdr xs) (cdr ys)))
      (cons (* (car xs) (car ys)) nil)))

; (mean xs) -- Calculates the mean value of a list of numbers.
(defun mean (xs)
  (/ (sum xs) (length xs)))

; (variance xs) -- Calculates the average variance from the mean of a 
;    list of numbers.
(defun variance (xs)
  (mean (listSqr (listMinusConst xs (mean xs)))))

; (scatter xs ys) -- Calculates the average of the variance between 
;    pairs of x and y coordinates and the respective means of those 
;    coordinate sets.
(defun scatter (xs ys)
  (mean (listXlist (listMinusConst xs (mean xs))
                   (listMinusConst ys (mean ys)))))

; (correlationSquared xs ys) -- Calculates the ratio between the square 
;    of the scatter value of a set of x and y coordinates and the 
;    product of those coordinates' variances.
(defun correlationSquared (xs ys)
  (/ (sqr (scatter xs ys)) (* (variance xs) (variance ys))))

; (beta1 xs ys) -- Calculates the slope of a least-squares line from a
;    collection of points stored as two true-lists of x and y
;    coordinates.
(defun beta1 (xs ys)
  (/ (scatter xs ys) (variance xs)))

; (beta0 xs ys) -- Calculates the y-intercept of the above 
;    least-squares line.
(defun beta0 (xs ys)
  (- (mean ys) (* (beta1 xs ys) (mean xs))))

; Takes the ceiling(log2(num)).
(defun log2-ceiling (num)
  (if (not (posp (- num 1)))
      0
      (+ (log2-ceiling (ceiling num 2)) 1)))

; Find an m such that y = x/4^m where 1/4 < y <= 1.
(defun range-reduce (x)
  (ceiling (log2-ceiling (ceiling x 1)) 2))

; Find the square root of x to one significant digit.
(defun sqrt-to-one-digit (x)
  (let* ((m (range-reduce x))
         (y (/ x (expt 4 m))))
    (* (/ (+ (* 2 y) 1) 3) (expt 2 m))))

; algorithm taken from http://mitpress.mit.edu/sicp/chapter1/node9.html
; Takes the square root of something using newton's method.
(defun newton-sqrt (n x y)
  (if (zp n)
      y
      (newton-sqrt (- n 1) x (/ (+ (/ x y) y) 2))))

; Guarantees n digits of presicion for the square root function.
(defun sqrt~ (n x)
  (newton-sqrt (log2-ceiling n) x (sqrt-to-one-digit x)))


(defun variance-sum (xs ys b0 b1)
  (if (consp xs)
      (+ (sqr (+ (first ys)
                 (- b0)
                 (- (* b1 (first xs)))))
         (variance-sum (rest xs) (rest ys) b0 b1))
      0))

(defconst *p-value-table* 
  '((1 (0 .1584 .3249 .5095 .7265 1 1.376 1.963 3.078 6.314))
    (2 (0 .1421 .2887 .4447 .6172 .8165 1.061 1.386 1.886 2.92))
    (3 (0 .1366 .2767 .4242 .5844 .7649 .9785 1.25 1.638 2.353))
    (4 (0 .1338 .2707 .4142 .5686 .7407 .941 1.19 1.533 2.132))
    (5 (0 .1322 .2672 .4082 .5594 .7267 .9195 1.156 1.476 2.015))
    (6 (0 .1311 .2648 .4043 .5534 .7176 .9057 1.134 1.44 1.943))
    (7 (0 .1303 .2632 .4015 .5491 .7111 .896 1.119 1.415 1.895))
    (8 (0 .1297 .2619 .3995 .5459 .7064 .8889 1.108 1.397 1.86))
    (9 (0 .1293 .261 .3979 .5435 .7027 .8834 1.1 1.383 1.833))
    (10 (0 .1289 .2602 .3966 .5415 .6998 .8791 1.093 1.372 1.812))
    (15 (0 .1278 .2579 .3928 .5357 .6912 .8662 1.074 1.341 1.753))
    (20 (0 .1273 .2567 .3909 .5329 .687 .86 1.064 1.325 1.725))
    (30 (0 .1267 .2556 .389 .53 .6828 .8538 1.055 1.31 1.697))
    (100000 (0 .1257 .2533 .3853 .5244 .6745 .8416 1.036 1.282 1.645))))

; Certainty index i cooresponds with (i*10) percent certainty
(defun t-value-row (degrees-of-freedom p-value-table)
  (let* ((p-value-row (first p-value-table))
         (row (first p-value-row))
         (values (second p-value-row)))
    (if (or (<= degrees-of-freedom row) (endp (rest p-value-table)))
        values
        (t-value-row degrees-of-freedom (rest p-value-table)))))

(defun calc-delta-without-t-value (estimates actuals prediction)
  (let* ((b0 (beta0 estimates actuals))
         (b1 (beta1 estimates actuals))
         (n (length estimates))
         (var (/ (variance-sum estimates actuals b0 b1)
                 (- n 2)))
         (standard-dev (sqrt~ 3 var))
         (xavg (mean estimates))
         (prediction-xavg2 (sqr (- xavg prediction)))
         (xi-xavg (listMinusConst estimates xavg))
         (sum-xi-xavg2 (sum (listSqr xi-xavg)))
         (range (* standard-dev
                   (sqrt~ 3 (+ 1
                               (/ n)
                               (/ prediction-xavg2
                                  sum-xi-xavg2))))))
    range))

(defun prediction-ranges-helper (projection delta t-values)
  (if (consp t-values)
      (cons (cons (- projection (* delta (first t-values)))
                  (+ projection (* delta (first t-values))))
            (prediction-ranges-helper projection delta (rest t-values)))
      nil))

;-----------------Predict Size-----------------------------------
;e - Historical size estimates: e1,e2,e3,....en
;s - Historical actual size: s1, s2, s3, .... sn
;x = size estimated for the current project from the file i/o
;returns the size of the new project.
(defun predict-size(e s x)
  (let ((b0 (beta0 e s))
        (b1 (beta1 e s)))
    (+ b0 (* b1 x))))

(defun prediction-ranges (estimates actuals prediction)
  (let ((delta (calc-delta-without-t-value estimates actuals prediction))
        (t-values (t-value-row (- (length estimates) 2)
                               *p-value-table*))
        (projection (predict-size estimates actuals prediction)))
    (prediction-ranges-helper projection delta t-values)))