;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
(require "IBitmap.lisp")
(require "IUtilities.lisp")
(module MBitmap
  ;(import IUtilities)
  (import IUtilities)
  
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
  
  #| numeral->nat(b,xs)
b: number
xs: list of numerals
return: number representation of the list of numerals
|#
  (defun numeral->nat (b xs)
    (if (endp xs)
        0
        (+ (car xs) (* b (numeral->nat b (cdr xs))))))
  
  #| nat->bytes(n)
n: a number
return: a list where each element is a byte (number mod 256)
|#
  (defun nat->bytes (n)
    (if (zp n)
        nil
        (cons (mod n 256)
              (nat->bytes (floor n 256)))))
  (defun replace-h (ist old new out)
    (if (endp ist) out
        (if (equal (car ist) old)
            (replace-h (cdr ist) old new (append out (list new)))
            (replace-h (cdr ist) old new (append out (list (car ist)))))))
  
  #| replace(ist,old,new)
ist: list
old: list
new: list
return: a list with ist appended to new
|#
  (defun replace (ist old new)
    (replace-h ist old new '()))
  
  (defun flatten-h (2d r)
    (if (endp 2d) r
        (flatten-h
         (cdr 2d)
         (append (replace (replace (car 2d) 0 255) 1 0) r))))
  
  #| flatten(2d)
2d: two dimensional list
return: one-dimensional representation of 2d
|#
  (defun flatten (2d)
    (flatten-h 2d '()))
  
  #| mash(l)
l: list
return: returns smaller list. If (car l) is true, then add to the output list
|#
  (defun mash (l) 
    (if (endp l) nil
        (if (car l)
            (cons (car l) (mash (cdr l)))
            (mash (cdr l)))))
  
  #| bytes->nat(xs)
xs: list of bytes
return: natural representation of byte list
|#
  (defun bytes->nat (xs)
    (numeral->nat 256 xs))
  
  #| zpr(lst size)
lst: list
size: desired size - number
return: returns a list with 0s padded to the end so that the output list is of length size
|#
  (defun zpr (lst size)
    (if (equal (len lst) size) lst
        (zpr (append lst '(0)) size)))
  
  #| triple(x)
x: object
return: returns a list with x repeated three times
|#
  (defun triple (x) (list x x x))
  
  (defun x3h (in out)
    (if (endp in) out
        (x3h (cdr in) (append out (triple (car in))))))
  
  #| x3(lst)
lst: list
return: returns a list where each element is repeated three times
|#
  (defun x3 (lst)
    (x3h lst '()))
  
  #| 8le(x)
x: object
return: returns a list where x is repeated 8 times
|#
  (defun 8le (x) (list x x x x x x x x))
  
  #| nat->4byte(nat)
nat: number
return: returns a list representation of the number where each element is a 4 byte block
|#
  (defun nat->4byte (nat)
    (zpr (nat->bytes nat) 4))
  
  (defun scale-by-8h2 (in out)
    (if (endp in) out
        (scale-by-8h2 (cdr in) (append out (8le (car in))))))
  
  (defun scale-by-8h (in out)
    (if (endp in) out
        (scale-by-8h (cdr in) (append out (8le (scale-by-8h2 (car in) '()))))))
  
  #| scale-by-8(matrix)
matrix: matrix data structure
return: a matrix where each element is scaled in x and y directions by 8
|#
  (defun scale-by-8 (matrix)
    (scale-by-8h matrix '()))
  
  (defun fixel-h (data rs out)
    (if (endp data) out
        (let* ((howmany (floor rs *bytes/pixel*))
               (bytes (x3 (mash (take howmany data))))
               (leftovers (nthcdr howmany data))
               (row (zpr bytes rs)))
          (fixel-h leftovers rs (append out row)))))
  
  #| fixel(data,rs)
data: data list
rs: list
return: returns a list of "pixel-ized" data
|#
  (defun fixel (data rs)
    (fixel-h data rs '()))
  
  #| createPadding(row,padding)
row: list
padding: number
return: a list with the desired padding added to it
|#
  (defun createPadding (row padding)
    (if (zp padding)
        nil
        (cons row (createPadding row (- padding 1)))))
  
  #| side-pad(matrix,sidepadding,height)
matrix: matrix data structure
sidepadding: number
height: number
return: matrix with side padding added
|#
  (defun side-pad (matrix sidepadding height)
    (if (< height 0)
        matrix
        (let* ((row (nth height matrix))
               (newrow (append (append sidepadding row) sidepadding))
               (newmatrix (put-nth height newrow matrix)))
          (side-pad newmatrix sidepadding (- height 1)))))
  
  #| add-padding(matrix,padding)
matrix: matrix data structure
padding: number
return: matrix with padding around all edges
|#
  (defun add-padding (matrix padding)
    (let* ((height (length matrix))
           (width (length (nth 0 matrix)))
           (row (createList (+ width (* 2 padding)) 0))
           (top&bottompadding (createPadding row padding))
           (sidepadding (createList padding 0))
           (matrixsidepadded (side-pad matrix sidepadding height)))
      (append (append top&bottompadding matrixsidepadded) top&bottompadding)))
  
  #| create-bitmap(matrix)
matrix: matrix data structure
return: creates a bitmap bitstring with the given matrix
|#
  (defun create-bitmap (matrix)
    (let* ((matrix (add-padding matrix 4))
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
