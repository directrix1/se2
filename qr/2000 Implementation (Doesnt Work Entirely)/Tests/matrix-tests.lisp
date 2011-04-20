(in-package "ACL2")
(include-book "testing" :dir :teachpacks)
(include-book "doublecheck" :dir :teachpacks)
(include-book "Matrix")

(defthm matrix-get-width-thm
  (implies (and (integerp row) (integerp column) (integerp value) (> row 0) (> column 0))
           (equal (1+ column) (get-width (set-point (empty-matrix) row column value)))))

(defthm matrix-get-height-thm
  (implies (and (integerp row) (integerp column) (integerp value) (> row 0) (> column 0))
           (equal (1+ row) (get-height (set-point (empty-matrix) row column value)))))

(defthm matrix-set-point-thm
  (implies (integerp value)
           (equal (flatten-matrix (set-point (empty-matrix) 2 2 'value)) '((0 0 0) (0 0 0) (0 0 value)))))

(defthm matrix-get-point-thm
  (implies (and (integerp row) (integerp column) (integerp value) (> row 0) (> column 0))
           (equal (get-point (set-point (empty-matrix) row column value) row column) value)))

(check-expect (get-width (empty-matrix)) 0)
(check-expect (get-height (empty-matrix)) 0)
(check-expect (empty-matrix? (empty-matrix)) 't)
(check-expect (flatten-matrix (set-point (empty-matrix) 2 2 1)) '((0 0 0) (0 0 0) (0 0 1)))
(check-expect (flatten-matrix (set-point (empty-matrix) 2 2 0)) '((0 0 0) (0 0 0) (0 0 0)))
(check-expect (get-height (set-point (empty-matrix) 2 2 1)) 3)
(check-expect (get-width (set-point (empty-matrix) 2 2 1)) 3)
(check-expect (get-point (set-point (empty-matrix) 2 2 0) 2 2) 0)
(check-expect (get-point (set-point (empty-matrix) 2 2 1) 2 2) 1)
;(check-expect (flatten-matrix (get-square (set-point (empty-matrix) 2 2 1) 1 1 2 2)) '((0 0) (0 1)))

(defproperty matrix-get-width-test :repeat 100
  (row :value (random-integer)
       column :value (random-integer)
       value :value (random-integer))
  (implies (and (integerp row) (integerp column) (integerp value) (> row 0) (> column 0))
           (equal (1+ column) (get-width (set-point (empty-matrix) row column value)))))

(defproperty matrix-get-height-test :repeat 100
  (row :value (random-integer)
       column :value (random-integer)
       value :value (random-integer))
  (implies (and (integerp row) (integerp column) (integerp value) (> row 0) (> column 0))
           (equal (1+ row) (get-height (set-point (empty-matrix) row column value)))))

(defproperty matrix-set-point-test :repeat 100
       (value :value (random-integer))
  (implies (integerp value)
           (equal (flatten-matrix (set-point (empty-matrix) 2 2 'value)) '((0 0 0) (0 0 0) (0 0 value)))))

(defproperty matrix-get-point-test :repeat 100
  (row :value (random-integer)
       column :value (random-integer)
       value :value (random-integer))
  (implies (and (integerp row) (integerp column) (integerp value) (> row 0) (> column 0))
           (equal (get-point (set-point (empty-matrix) row column value) row column) value)))