;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#| Team Steele
   Software Engineering I
   MpngQR

   Functions to construct PNG files from QR matrices.
|#
(in-package "ACL2")

(require "IpngQR.lisp")
(require "Izlib.lisp")
(require "IpngUtils.lisp")

(module MpngQR
  
  (import IpngUtils)
  (import Izlib)
  
  ; PNG signature
  (defconst *png-sig* '(137 80 78 71 13 10 26 10))
  
  ; Returns a list composed of x n times where
  ;  x = the value to repeat
  ;  n = the number of times to repeat x
  (defun repeat-ntimes (x n)
    (if (zp n)
        nil
        (cons x (repeat-ntimes x (1- n)))))

  ; Returns a matrix with 4 pixel buffer on left and right sides where:
  ;  matrix = a list of lists like '((1 0 1 1 ... 0 1)
  ;                                  . . . . . . . . .
  ;                                  (0 1 1 0 ... 1 0))  
  (defun bufferRowsBy4 (matrix)
    (if (endp matrix)
        nil
        (cons (concatenate 'list
                           (repeat-ntimes 0 4)
                           (car matrix)
                           (repeat-ntimes 0 4))
              (bufferRowsBy4 (cdr matrix)))))

  ; Returns a matrix with 4 pixel buffer on each side where:
  ;  matrix = a list of lists like '((1 0 1 1 ... 0 1)
  ;                                  . . . . . . . . .
  ;                                  (0 1 1 0 ... 1 0))
  (defun bufferMatrixBy4 (matrix)
    (let ((bufferrows 
           (repeat-ntimes
            (repeat-ntimes 0 (+ 8 (len (car matrix))))
            4)))
      (concatenate 'list                 
                   bufferrows
                   (bufferRowsBy4 matrix)
                   bufferrows)))
    
  ; Returns a row scaled by 8 horizontally and multiplied
  ; by 255, where:
  ;  row = a list like '(1 0 1 1 ... 0 1)
  (defun scalifyRow (row)
    (if (null row)
		nil
		(let ((value (* 255 (- 1 (car row)))))
			(append 
				(repeat-ntimes value 8)
			 	(scalifyRow (cdr row))))))
    
  ; Returns the matrix scaled by 8 in each direction and multiplied
  ; by 255, where:
  ;  matrix = a list of lists like '((1 0 1 1 ... 0 1)
  ;                                  . . . . . . . . .
  ;                                  (0 1 1 0 ... 1 0))
  (defun scalify (matrix)
    (if (null matrix)
		nil
                (let ((row (scalifyRow (car matrix))))
                  (append
                   (list row row row row row row row row)
                   (scalify (cdr matrix))))))

  ; Adds filter type 0 to the beginning of each row in the matrix where:
  ;  matrix = a list of lists like '((1 0 1 1 ... 0 1)
  ;                                  . . . . . . . . .
  ;                                  (0 1 1 0 ... 1 0))  
  (defun addFilter (matrix)
    (if (endp matrix)
        nil
        (cons (cons 0 (car matrix))
              (addFilter (cdr matrix)))))
  
  ; Concatenates a list of byte lists into one long list where:
  ;  bytelists = a list of lists like '((5 0 253 20 ... 0 1)
  ;                                      . . . . . . . . .
  ;                                     (0 1 7 0 ... 9 0))  
  (defun concatByteLists (bytelists)
    (if (endp bytelists)
        nil
        (concatenate 'list 
                     (car bytelists)
                     (concatBytelists (cdr bytelists)))))
  
  ; Creates an IDAT chunk when given a matrix where:
  ;  matrix = a list of lists like '((1 0 1 1 ... 0 1)
  ;                                  . . . . . . . . .
  ;                                  (0 1 1 0 ... 1 0))  
  (defun makeIDAT (matrix)
    (makeChunk "IDAT" 
               (zlib-compress 
                (concatByteLists (addFilter (scalify matrix)))
                'toinfinityandbeyond)))
               
    
  
  ; Creates a grayscale IHDR chunk with the specified width and height
  ; where:
  ;  width = the width of the PNG image
  ;  height = the height of the PNG image
  (defun makeIHDR (width height)
    (makeChunk "IHDR"
               (concatenate 'list
                            (makeNum width nil 4)
                            (makeNum height nil 4)
                            (makeNum 8 nil 1)
                            (makeNum 0 nil 1)
                            (makeNum 0 nil 1)
                            (makeNum 0 nil 1)
                            (makeNum 0 nil 1))))
  
  ; Creates a PNG from the specified matrix where:
  ;  matrix = a list of lists like '((1 0 1 1 ... 0 1)
  ;                                  . . . . . . . . .
  ;                                  (0 1 1 0 ... 1 0))    
  (defun makePNG (matrix)
    (let* ((bmatrix (bufferMatrixBy4 matrix))
           (width (ash (len (car bmatrix)) 3))
           (height (ash (len bmatrix) 3)))
      (concatenate 'list
                   *png-sig*
                   (makeIHDR width height)
                   (makeIDAT bmatrix)
                   (makeChunk "IEND" nil))))
  
  (export IpngQR))