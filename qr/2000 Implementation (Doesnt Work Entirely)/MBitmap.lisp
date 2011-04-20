;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")

(require "IBitmap.lisp")
(require "IMatrix.lisp")

(module MBitmap
  
  (import IMatrix)
  
  ;values for QR image colors
  (defconst *white* 0)
  (defconst *black* 1)
  
  ;decimal values of offsets in bitmap format
  (defconst *offset* 10)
  (defconst *width* 18)
  (defconst *height* 22)
  (defconst *bpp* 28)
  
  (defconst *pixels/module* 8)
  (defconst *bytes/pixel* 3)
  (defconst *4bytes* '(0 0 0 0))
  (defconst *type* '(66 77))
  (defconst *px-offset* '(54 0 0 0))
  (defconst *dib-size* '(40 0 0 0))
  (defconst *dib-planes* '(1 0))
  (defconst *dib-bpp* '(24 0))
  (defconst *dib-comp* '(0 0 0 0))
  (defconst *dib-hres* '(19 11 0 0))
  (defconst *dib-vres* '(19 11 0 0))
  (defconst *dib-palette* '(0 0 0 0))
  (defconst *dib-colors* '(0 0 0 0))
  
  ;private functions
  
  ;reused
  (defun numeral->nat (b xs)
    (if (endp xs)
        0
        (+ (car xs) (* b (numeral->nat b (cdr xs))))))
  
  (defun nat->bytes (n)
    (if (zp n)
        nil
        (cons (mod n 256)
              (nat->bytes (floor n 256)))))
  
  (defun bytes->nat (xs)
    (numeral->nat 256 xs))
  ;/reused
  
  ;list utilities
  (defun list-get (n o r xs)
    (if (and
         (true-listp xs)
         (posp n)
         (<= (+ n o) (len xs)))
        (if r
            (list (take n (nthcdr o xs)) (nthcdr (+ n o) xs))
            (take n (nthcdr o xs)))
        nil))
  
  (defun partition (s l)
    (if (endp l)
        nil
        (cons (take s l) (partition s (nthcdr s l)))))
  
  (defun every-nth (n l)
    (if (< (len l) n)
        nil
        (cons (nth (- n 1) l) (every-nth n (nthcdr n l)))))
  
  (defun until-helper (v l i)
    (if (endp l)
        nil
        (if (equal (car l) v)
            i
            (until-helper v (cdr l) (+ i 1)))))
  
  (defun until (v l)
    (until-helper v l 0))
  
  (defun mash (l) 
    (if (endp l) nil
        (if (car l)
            (cons (car l) (mash (cdr l)))
            (mash (cdr l)))))
  ;/list utilities
  
  ;bitmap parsing functions
  (defun fetch-bytes (bytes size offset)
    (bytes->nat (list-get size offset nil bytes)))
  
  (defun calc-rowsize (bpp width)
    (* (ceiling (* bpp width) 32) 4))
  
  (defun calc-px-cnt (rowsize height)
    (* rowsize (abs height)))
  
  (defun calc-pad (rowsize bypp)
    (- rowsize (* bypp (floor rowsize bypp))))
  
  (defun rows->pixels (rows bypp pad)
    (if (endp rows)
        nil
        (append (rows->pixels (cdr rows) bypp pad)
                (partition bypp (butlast (car rows) pad)))))
  
  (defun pixels->bits (pxs white)
    (if (endp pxs)
        nil
        (if (equal (car pxs) white)
            (cons *white* (pixels->bits (cdr pxs) white))
            (cons *black* (pixels->bits (cdr pxs) white)))))
  
  (defun find-left-corner-helper (bits row)
    (if (endp bits)
        nil
        (if (until *black* (car bits))
            row
            (find-left-corner-helper (cdr bits) (+ row 1)))))
  
  (defun find-left-corner (bits)
    (find-left-corner-helper bits 0))
  
  (defun reduce-bits (bits unit)
    (if (or (endp bits)
            (< (len bits) unit))
        nil
        (cons (every-nth unit (nth (- unit 1) bits))
              (reduce-bits (nthcdr unit bits) unit))))
  ;/bitmap parsing functions
  
  ;unit-array to matrix functions
  (defun make-cols (R M i j s)
    (if (endp R) M
        (let* ((value (car R))
               (r# (- s i))
               (c# (- s j))
               (M+ (mset M r# c# value)))
          (make-cols (cdr R) M+ i (- j 1) s))))
  
  (defun make-rows-h (D M i s)
    (if (endp D) M
        (let* ((R (car D))
               (M+ (make-cols R M i (len R) s)))
          (make-rows-h (cdr D) M+ (- i 1) s))))
  
  (defun make-rows (D M)
    (make-rows-h D M (len D) (len D)))
  ;/unit-array to matrix functions
  
  ;2 5 9 19 49
  ;start at 1 am end 4:30
  (defun zpr (lst size)
    (if (equal (len lst) size) lst
        (zpr (append lst '(0)) size)))
  
  (defun replace-h (ist old new out)
    (if (endp ist) out
        (if (equal (car ist) old)
            (replace-h (cdr ist) old new (append out (list new)))
            (replace-h (cdr ist) old new (append out (list (car ist)))))))
  
  (defun replace (ist old new)
    (replace-h ist old new '()))
  
  (defun flatten-h (2d r)
    (if (endp 2d) r
        (flatten-h
         (cdr 2d)
         (append (replace (replace (car 2d) 0 255) 1 0) r))))
  
  (defun flatten (2d)
    (flatten-h 2d '()))
  
  (defun triple (x) (list x x x))
  
  (defun 8le (x) (list x x x x x x x x))
  
  (defun x3h (in out)
    (if (endp in) out
        (x3h (cdr in) (append out (triple (car in))))))
  
  (defun x3 (lst)
    (x3h lst '()))
  
  (defun scale-by-8h2 (in out)
    (if (endp in) out
        (scale-by-8h2 (cdr in) (append out (8le (car in))))))
  
  (defun scale-by-8h (in out)
    (if (endp in) out
        (scale-by-8h (cdr in) (append out (8le (scale-by-8h2 (car in) '()))))))
  
  (defun scale-by-8 (matrix)
    (scale-by-8h matrix '()))
  
  (defun nat->4byte (nat)
    (zpr (nat->bytes nat) 4))
  
  (defun fixel-h (data rs out)
    (if (endp data) out
        (let* ((howmany (floor rs *bytes/pixel*))
               (bytes (x3 (mash (take howmany data))))
               (leftovers (nthcdr howmany data))
               (row (zpr bytes rs)))
          (fixel-h leftovers rs (append out row)))))
  
  (defun fixel (data rs)
    (fixel-h data rs '()))
  
  ;Exported functions
  (defun parse-bitmap (bytes)
    (let* ((offset (fetch-bytes bytes 4 *offset*))
           (width (fetch-bytes bytes 4 *width*))
           (height (fetch-bytes bytes 4 *height*))
           (bpp (fetch-bytes bytes 2 *bpp*))
           (rowsize (calc-rowsize bpp width))
           (px-cnt (calc-px-cnt rowsize height))
           (bypp (/ bpp 8))
           (pad (calc-pad rowsize bypp))
           (px-data (list-get px-cnt offset nil bytes))
           (rows (partition rowsize px-data))
           (pxs (rows->pixels rows bypp pad))
           (white (car pxs))
           (bits (partition width (pixels->bits pxs white)))
           (target (nth (find-left-corner bits) bits))
           (start (until *black* target))
           (block (until *white* (nthcdr start target)))
           (size (/ block 7))
           (unit (reduce-bits bits size)))
      (make-rows unit (empty-matrix))))
  
  (defun create-bitmap (matrix)
    (let* ((matrix (flatten-matrix matrix))
		   (side (* (len matrix) *pixels/module*))
           (size (* side side))
           (rowsize (* (ceiling (* (bytes->nat *dib-bpp*) side) 32) 4))
           (pixels (fixel (flatten (scale-by-8 matrix)) rowsize))    
           (head (append
                  *type*
                  (nat->4byte (+ (len pixels) 54))
                  *4bytes*
                  *px-offset*))
           (dib (append
                 *dib-size*
                 (nat->4byte side)
                 (nat->4byte side)
                 *dib-planes*
                 *dib-bpp*
                 *dib-comp*
                 (nat->4byte size)
                 *dib-hres*
                 *dib-vres*
                 *dib-palette*
                 *dib-colors*))
           (image (append head dib pixels)))
      image))
  
  (export IBitmap))