(in-package "ACL2")

(include-book "list-utilities" :dir :teachpacks)
(include-book "io-utilities" :dir :teachpacks)
(include-book "avl-rational-keys" :dir :teachpacks)			

(defun put-nth (n v l)
  (declare (xargs :guard (and (integerp n) (<= 0 n)
                              (true-listp l))))
  (cond ((endp l) nil)
        ((zp n) (cons v (cdr l)))
        (t (cons (car l) (put-nth (1- n) v (cdr l))))))

(defun createListLoop (n v l)
  (if (zp n)
      l
      (createListLoop (- n 1) v (cons v l))))

(defun createList (n v)
  (createListLoop n v nil))

(defun getBytesLoop1 (content i)
  (if (< i (length content))
      (cons 
       (char-code (nth i content)) 
       (getBytesLoop1 content (+ i 1)))
      nil))

(defun getBytes (content)
  (getBytesLoop1 (str->chrs content) 0))
  
(defun intTobin (from to num)
  (if (>= from to)
      (cons (if (logbitp from num) 1 0) 
            (intTobin (- from 1) to num))
      nil))

(defun int->bin (num)
  (intTobin 31 0 num))

(defun binToint (from to num)
  (if (< from to)
      (+ (* (nth from num) (expt 2 (- 31 from))) 
         (binToint (+ from 1 ) to num))
      0))

(defun bin->int (num)
  (let* ((int (binToint 0 32 num)))
    (if (< int 2147483647)
        int
        (- int 4294967296))))

(defun normalizei (i)
  (if (> i 32)
      (normalizei (- i 32))
      (if (< i 0)
          (normalizei (+ i 32))
          i)))

(defun << (num i)
  (let* ((binarynum (int->bin num))
         (normalizedi (normalizei i))
         (shifted (nthcdr normalizedi binarynum))
         (padding (createList normalizedi 0)))
    (bin->int (append shifted padding))))

(defun >> (num i)
  (let* ((binarynum (int->bin num))
         (normalizedi (normalizei i))
         (sign (car binarynum))
         (shifted (take (- 32 normalizedi) binarynum))
         (padding (createList normalizedi sign)))
    (bin->int (append padding shifted))))

(defun >>> (num i)
  (let* ((binarynum (int->bin num))
         (normalizedi (normalizei i))
;         (sign (car binarynum))
         (shifted (take (- 32 normalizedi) binarynum))
         (padding (createList normalizedi 0)))
    (bin->int (append padding shifted))))