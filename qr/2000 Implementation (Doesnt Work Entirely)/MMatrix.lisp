;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
(require "IMatrix.lisp")

(module MMatrix
  
  (include-book "avl-rational-keys" :dir :teachpacks)
  
  (defun empty-matrix? (matrix) (not (consp matrix)))
  
  (defun empty-matrix () (list 0 0 nil))
  
  (defun get-width (matrix)
    (nth 0 matrix))
  (defun get-height (matrix)
    (nth 1 matrix))
  
  (defun mset (matrix row column value)
    (let* ((oldwidth (get-width matrix))
           (width (if (> (+ column 1) oldwidth) (+ column 1) oldwidth))
           (oldheight (get-height matrix))
           (height (if (> (+ row 1) oldheight) (+ row 1) oldheight))
           (datum (nth 2 matrix))
           (previous-value (cdr (avl-retrieve (cdr (avl-retrieve datum column)) row)))
           (newdatum (if (and (null previous-value) (equal value 0)) datum (avl-insert datum column (avl-insert (cdr (avl-retrieve datum column)) row value)))))
      (list width height newdatum)))
  
  (defun mget (matrix row column)
    (let* ((width (get-width matrix))
           (height (get-height matrix))
           (datum (nth 2 matrix)))
      (if (or (> column (- width 1)) (> row (- height 1)))
          nil
          (let* ((value (cdr (avl-retrieve (cdr (avl-retrieve datum column)) row))))
            (if (null value)
                0
                value)))))
  
  (defun square (matrix tlRow tlColumn brRow brColumn output row col)
    (if (> col brColumn)
        output
        (let* ((value (mget matrix row col))
               (partialSquare (mset output (- row tlRow) (- col tlColumn) value)))
          (if (equal row brRow)
              (square matrix tlRow tlColumn brRow brColumn partialSquare tlRow (+ col 1))
              (square matrix tlRow tlColumn brRow brColumn partialSquare (+ row 1) col)))))
  (defun get-square (matrix tlRow tlColumn brRow brColumn)
    (square matrix tlRow tlColumn brRow brColumn (empty-matrix) tlRow tlColumn))
  
  (defun get-row (matrix row column)
    (if (equal column (get-width matrix))
        nil
        (cons (mget matrix row column) (get-row matrix row (+ column 1)))))
  (defun iterate-through-rows (matrix row)
    (if (equal row (get-height matrix))
        nil
        (cons (get-row matrix row 0) (iterate-through-rows matrix (+ row 1)))))
  (defun flatten-matrix (matrix)
    (iterate-through-rows matrix 0))
  
  (export IMatrix))