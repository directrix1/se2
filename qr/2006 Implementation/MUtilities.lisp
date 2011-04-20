;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
(require "IUtilities.lisp")
(module MUtilities
  (include-book "list-utilities" :dir :teachpacks)
  (include-book "io-utilities" :dir :teachpacks)
  (include-book "avl-rational-keys" :dir :teachpacks)
  
  #| put-nth(n v l)
n: number
v: anything
l: list
returns a new list with v put into the nth place of l
|#
  (defun put-nth (n v l)
    (declare (xargs :guard (and (integerp n) (<= 0 n)
                                (true-listp l))))
    (cond ((endp l) nil)
          ((zp n) (cons v (cdr l)))
          (t (cons (car l) (put-nth (1- n) v (cdr l))))))
  
  #| createListLoop(n,v,l)
n: number
v: anything
l: list
return: list with v appended n times to the front of l
|#
  (defun createListLoop (n v l)
    (if (zp n)
        l
        (createListLoop (- n 1) v (cons v l))))
  
  #| createList(n,v)
n: number
v: object
return: creates a list with n copies of v
|#
  (defun createList (n v)
    (createListLoop n v nil))
  
  (defun getBytesLoop1 (content i)
    (if (< i (length content))
        (cons 
         (char-code (nth i content)) 
         (getBytesLoop1 content (+ i 1)))
        nil))
  
  #| getBytes(content)
content: character string
return: list where each element is the character code of each character in the string
|#
  (defun getBytes (content)
    (getBytesLoop1 (str->chrs content) 0))
  
  (defun intTobin (from to num)
    (if (>= from to)
        (cons (if (logbitp from num) 1 0) 
              (intTobin (- from 1) to num))
        nil))
  
  #| int->bin(num)
num: a number
return: list of binary digits representing num
|#
  (defun int->bin (num)
    (intTobin 31 0 num))
  
  (defun binToint (from to num)
    (if (< from to)
        (+ (* (nth from num) (expt 2 (- 31 from))) 
           (binToint (+ from 1 ) to num))
        0))
  
  #| bin->int(num)
num: number
return: integer representing num
|#
  (defun bin->int (num)
    (let* ((int (binToint 0 32 num)))
      (if (< int 2147483647)
          int
          (- int 4294967296))))
  
  #| normalizei(i)
i: number
return: normalizes a number to 32-bits
|#
  (defun normalizei (i)
    (if (> i 32)
        (normalizei (- i 32))
        (if (< i 0)
            (normalizei (+ i 32))
            i)))
  
  #| <<(num,i)
num: number
i: number
return: bit-shifts num left by i bit-shifts
|#
  (defun << (num i)
    (let* ((binarynum (int->bin num))
           (normalizedi (normalizei i))
           (shifted (nthcdr normalizedi binarynum))
           (padding (createList normalizedi 0)))
      (bin->int (append shifted padding))))
  
  #| >>(num,i)
num: number
i: number
return: bit-shifts num right by i bit-shifts
|#
  (defun >> (num i)
    (let* ((binarynum (int->bin num))
           (normalizedi (normalizei i))
           (sign (car binarynum))
           (shifted (take (- 32 normalizedi) binarynum))
           (padding (createList normalizedi sign)))
      (bin->int (append padding shifted))))
  
  #| >>>(num,i)
num: number
i: number
return: bit-shirts num right by i bit-shifts
|#
  (defun >>> (num i)
    (let* ((binarynum (int->bin num))
           (normalizedi (normalizei i))
           (sign (car binarynum))
           (shifted (take (- 32 normalizedi) binarynum))
           (padding (createList normalizedi 0)))
      (bin->int (append padding shifted))))
  
  (export IUtilities))