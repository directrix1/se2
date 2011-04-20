;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
(require "IMatrix.lisp")
(require "IUtilities.lisp")
(module MMatrix
  (import IUtilities)
  
  #|BitArray:makeArray(size)
size: int
returns: list
Make a list of elements
|#
  (defun BitArray:makeArray (size)
    (createList (>> (+ size 31) 5) 0))
  
  #|BitArray()
empty constructor
|#
  (defun BitArray ()
    (list "BitArray" 0 (list 0)))
  
  #|BitArray$1(size)
constructor for BitArray with one paramater
|#
  (defun BitArray$1 (size)
    (list "BitArray" size (BitArray:makeArray size)))
  
  (defconst *BitArray:SIZE* 1)
  (defconst *BitArray:BITS* 2)
  
  #|BitArray:getSize(ba)
ba: list
return: int
get size of the BitArray
|#
  (defun BitArray:getSize (ba)
    (nth *BitArray:SIZE* ba))
  
  (defun BitArray:getSizeInBytes (ba)
    (>> (+ (nth *BitArray:SIZE* ba) 7) 3))
  
  #|BitArray:ensureCapacity(ba size)
ba: list
size: int
return: list
ensures the size of the array stays a specific length.
|#
  (defun BitArray:ensureCapacity (ba size)
    (let* ((bits (nth *BitArray:BITS* ba)))
      (if (> size (<< (length bits) 5))
          (let* ((newBits (append bits (createList (- size (length bits)) 0))))
            (put-nth *BitArray:BITS* newBits ba))
          ba)))
  
  #|BitArray:get(ba i)
ba: list
i: int
return:int
Retrives a single bit from the BitArray
|#  
  (defun BitArray:get (ba i)
    (let* ((bits (nth *BitArray:BITS* ba)))
      (not (equal (logand (nth (>> i 5) bits) (<< 1 (logand i 31))) 0))))
  
  #|BitArray:set(ba i)
ba: list
i: int
return: list
set a single bit in the BitArray
|# 
  (defun BitArray:set (ba i)
    (let* ((bits (nth *BitArray:BITS* ba))
           (offset (>> i 5))
           (value (nth offset bits))
           (newvalue (logior value (<< 1 (logand i 31))))
           (newbits (put-nth offset newvalue bits)))
      (put-nth *BitArray:BITS* newbits ba)))
  
  #|BitArray:flip(ba i)
ba: list
i: int
return: list
flips a single bit in the BitArray
|#  
  (defun BitArray:flip (ba i)
    (let* ((bits (nth *BitArray:BITS* ba))
           (offset (>> i 5))
           (value (nth offset bits))
           (newvalue (logxor value (<< 1 (logand i 31))))
           (newbits (put-nth offset newvalue bits)))
      (put-nth *BitArray:BITS* newbits ba)))
  
  #|BitArray:setBulk (ba i newBits)
ba: list
i: int
newBits: list
return: list
sets a large number of bits in a BitArray after index given
|#
  (defun BitArray:setBulk (ba i newBits)
    (let* ((bits (nth *BitArray:BITS* ba))
           (offset (>> i 5)))
      (put-nth *BitArray:BITS* (put-nth offset newBits bits) ba)))
  
  #|BitArray:clear(ba)
ba:list
:return:list
clears list of bits in the BitArray
|# 
  (defun BitArray:clear (ba)
    (let* ((bits (nth *BitArray:BITS* ba))
           (newbits (createList (length bits) 0)))
      (put-nth *BitArray:BITS* newbits ba)))
  
  #|rangeloops
helper function for BitArray:isRange
|#
  (defun BitArray:isRangeLoop2 (mask j lastBit)
    (if (<= j lastBit)
        (BitArray:isRangeLoop2 (logior mask (<< 1 j)) (+ j 1) lastBit)
        mask))
  
  #|rangeloops
helper function for BitArray:isRange
|#
  (defun BitArray:isRangeLoop1 (bits start end value firstInt lastInt i)
    (if (<= i lastInt)
        (let* ((firstBit (if (> i firstInt) 0 (logand start 31)))
               (lastBit (if (< i lastInt) 31 (logand end 31)))
               (mask (if (and (equal firstBit 0) (equal lastBit 31)) -1 0))
               (finalmask (BitArray:isRangeLoop2 mask firstBit lastBit)))
          (if (not (equal 
                    (logand (nth i bits) finalmask) 
                    (if value finalmask 0)))
              nil
              (BitArray:isRangeLoop1 
               bits start end value firstInt lastInt (+ i 1))))
        t))
  
  #|BitArray:isRange(ba start end value)
ba:list
start: int
end: int
value:int
checks if in range
|#
  (defun BitArray:isRange (ba start end value)
    (if (equal end start)
        t
        (let* ((bits (nth *BitArray:BITS* ba))
               (firstInt (>> start 5))
               (lastInt (>> (- end 1) 5)))
          (BitArray:isRangeLoop1 
           bits start (- end 1) value firstInt lastInt firstInt))))
  
  #|BitArray:appendBit(ba bit)
ba:list
bit:int
return:list
as the name suggest it appends a bit
|#
  (defun BitArray:appendBit (ba bit)
    (let* ((size (nth *BitArray:SIZE* ba))
           (correctSize (BitArray:ensureCapacity ba (+ size 1)))
           (bits (nth *BitArray:BITS* correctSize))
           (offset (>> size 5))
           (value (nth offset bits))
           (newvalue (if bit (logior value (<< 1 (logand size 31))) value))
           (newbits (put-nth offset newvalue bits)))
      (list "BitArray" (+ size 1) newbits)))
  
  #|BitArray:appendBits
ba:list
balue:int
numBits:int
return:list
appends multiple bits
|#
  (defun BitArray:appendBits (ba value numBits)
    (if (> numBits 0)
        (let* ((bit (equal (logand (>> value (- numBits 1)) 1) 1)))
          (BitArray:appendBits 
           (BitArray:appendBit ba bit) 
           value (- numBits 1)))
        ba))
  
  #|BitArray:appendBitArrayLoop1
helper function for appendBitArray
|#
  (defun BitArray:appendBitArrayLoop1 (ba other otherSize i)
    (if (< i otherSize)
        (BitArray:appendBitArrayLoop1 
         (BitArray:appendBit ba (BitArray:get other i)) 
         other otherSize (+ i 1))
        ba))
  
  #|BitArray:appendBitArray(ba other)
ba:list
other:list
return:list
appends a BitArray
|#  
  (defun BitArray:appendBitArray (ba other)
    (let* ((size (nth *BitArray:SIZE* ba))
           (otherSize (nth *BitArray:SIZE* other))
           (bits (nth *BitArray:BITS* ba))
           (otherbits (nth *BitArray:BITS* other)))
      (BitArray:appendBitArrayLoop1 ba other otherSize 0)))
  
  #|BitArray:xorLoop1
help function for BitArray:xor
|#
  (defun BitArray:xorLoop1 (bits otherbits i)
    (if (< i (length bits))
        (cons (logxor (nth i bits) (nth i otherbits)) 
              (BitArray:xorLoop1 bits otherbits (+ i 1)))
        nil))
  
  #|BitArray:xor (ba other)
ba: list
other:list
return: list
used to xor a BitArray
|#
  (defun BitArray:xor (ba other)
    (let* ((bits (nth *BitArray:BITS* ba))
           (otherbits (nth *BitArray:BITS* other))
           (newbits (BitArray:xorLoop1 bits otherbits 0)))
      (put-nth *BitArray:BITS* newbits ba)))
  
  #|toBytesLoops
helper function for BitArray:toBytes
|#
  (defun BitArray:toBytesLoop2 (ba bitOffset theByte j)
    (if (< j 8)
        (let* ((newByte (if (BitArray:get ba bitOffset)
                            (logior theByte (<< 1 (- 7 j)))
                            theByte)))
          (BitArray:toBytesLoop2 ba (+ bitOffset 1) newByte (+ j 1)))
        (if (> theByte 128)
            (- theByte 256)
            theByte)))
  
  #|toBytesLoops
helper function for BitArray:toBytes
|#
  (defun BitArray:toBytesLoop1 (ba bitOffset array offset numBytes i)
    (if (< i numBytes)
        (let* ((theByte (BitArray:toBytesLoop2 ba bitOffset 0 0))
               (newarray (put-nth (+ offset i) theByte array)))
          (BitArray:toBytesLoop1 
           ba (+ bitOffset 8) newarray offset numBytes (+ i 1)))
        array))
  
  #|BitArray:toBytes
ba:list
bitOffset:int
array:list
offset:int
numBytes:int
returns:list
Changes to a set of bytes
|#
  (defun BitArray:toBytes (ba bitOffset array offset numBytes)
    (BitArray:toBytesLoop1 ba bitOffset array offset numBytes 0))
  
  #|BitArray:getBitArray
ba:list
returns list
gets the bit Array
|#
  (defun BitArray:getBitArray (ba)
    (nth *BitArray:BITS* ba))
  
  #|reverseLoop1
helper function for BitArray:Reverse
|#
  (defun BitArray:reverseLoop1 (ba bits size i)
    (if (< i size)
        (let* ((newbits 
                (if (BitArray:get ba (+ size (* -1 i) -1))
                    (let* ((offset (>> i 5))
                           (previousvalue (nth offset bits))
                           (value (logior previousvalue 
                                          (<< 1 (logand i 31)))))
                      (put-nth offset value bits))
                    bits)))
          (BitArray:reverseLoop1 ba newbits size (+ i 1)))
        (put-nth *BitArray:BITS* bits ba)))
  
  #|BitArray:reverse
ba:list
return:list
reverse of the bit Array
|#
  (defun BitArray:reverse (ba)
    (let* ((size (nth *BitArray:SIZE* ba))
           (bits (nth *BitArray:BITS* ba))
           (newBits (createList (length bits) 0)))
      (BitArray:reverseLoop1 ba newbits size 0)))
  
  #|BitMatrix$2
width:int
height:int
return:list
constructor for BitMatrix takes 2 elements
|#
  (defun BitMatrix$2 (width height)
    (let* ((rowSize (>> (+ width 31) 5))
           (bits (createList (* rowSize height) 0)))
      (list "BitMatrix" width height rowSize bits)))
  
  #|BitMatrix$1
dimensions:int
constructor for BitMatrix takes 1 element
|#
  (defun BitMatrix$1 (dimension)
    (BitMatrix$2 dimension dimension))
  
  (defconst *BitMatrix:WIDTH* 1)
  (defconst *BitMatrix:HEIGHT* 2)
  (defconst *BitMatrix:ROW_SIZE* 3)
  (defconst *BitMatrix:BITS* 4)
  
  #|BitMatrix:get(bm x y)
bm:list
x:int
y:int
return:int
get bit in the matrix
|#
  (defun BitMatrix:get (bm x y)
    (let* ((rowSize (nth *BitMatrix:ROW_SIZE* bm))
           (bits (nth *BitMatrix:BITS* bm))
           (offset (+ (* y rowSize) (>> x 5))))
      (not (equal (logand (>>> (nth offset bits) (logand x 31)) 1) 0))))
  
  #|BitMatrix:set
bm:list
x:int
y:int
return:list
sets a bit in the matrix
|#
  (defun BitMatrix:set (bm x y)
    (let* ((rowSize (nth *BitMatrix:ROW_SIZE* bm))
           (bits (nth *BitMatrix:BITS* bm))
           (offset (+ (* y rowSize) (>> x 5)))
           (previousvalue (nth offset bits))
           (newvalue (logior previousvalue (<< 1 (logand x 31)))))
      (put-nth *BitMatrix:BITS* (put-nth offset newvalue bits) bm)))
  
  #|BitMatrix:flip
bm:list
x:int
y:int
return:list
flips the bit in the matrix
|#
  (defun BitMatrix:flip (bm x y)
    (let* ((rowSize (nth *BitMatrix:ROW_SIZE* bm))
           (bits (nth *BitMatrix:BITS* bm))
           (offset (+ (* y rowSize) (>> x 5)))
           (previousvalue (nth offset bits))
           (newvalue (logxor previousvalue (<< 1 (logand x 31)))))
      (put-nth *BitMatrix:BITS* (put-nth offset newvalue bits) bm)))
  
  #|BitMatrix:clear
bm:list
return:list
clear the Matrix
|#
  (defun BitMatrix:clear (bm)
    (let* ((bits (nth *BitMatrix:BITS* bm))
           (newbits (createList (length bits) 0)))
      (put-nth *BitMatrix:BITS* newbits bm)))
  
  #|region loop
helper function for region
|#
  (defun BitMatrix:setRegionLoop1 (rowSize bits top bottom left right x y)
    (if (< y bottom)
        (let* ((offset (* y rowSize)))
          (if (< x right)
              (let* ((i (+ offset (>> x 5)))
                     (value (nth i bits))
                     (newvalue (logior value (<< 1 (logand x 31))))
                     (newbits (put-nth i newvalue bits)))
                (BitMatrix:setRegionLoop1 
                 rowSize newbits top bottom left right (+ x 1) y))
              (BitMatrix:setRegionLoop1 
               rowSize bits top bottom left right left (+ y 1))))
        bits))
  
  #|BitMatrix:setRegion
bm:list
left:int
top:int
width:int
height:int
return:list
set special region
|#
  (defun BitMatrix:setRegion (bm left top width height)
    (let* ((rowSize (nth *BitMatrix:ROW_SIZE* bm))
           (right (+ left width))
           (bottom (+ top height))
           (bits (nth *BitMatrix:BITS* bm))
           (newbits (BitMatrix:setRegionLoop1 
                     rowSize bits top bottom left right left top)))
      (put-nth *BitMatrix:BITS* newbits bm)))
  
  #|rowloop
getRow helper function
|#
  (defun BitMatrix:getRowLoop1 (row rowSize bits offset x)
    (if (< x rowSize)
        (let* ((newrow 
                (BitArray:setBulk row (<< x 5) (nth (+ offset x) bits))))
          (BitMatrix:getRowLoop1 newrow rowSize bits offset (+ x 1)))
        row))
  
  #|BitMatrix:getRow
bm:list
y:int
ba:list
return:list
gets the row in a matrix
|#
  (defun BitMatrix:getRow (bm y ba)
    (let* ((width (nth *BitMatrix:WIDTH* bm))
           (rowSize (nth *BitMatrix:ROW_SIZE* bm))
           (bits (nth *BitMatrix:BITS* bm))
           (row (if (or (null ba) (< (BitArray:getSize ba) width))
                    (BitArray$1 width)
                    ba))
           (offset (* y rowSize)))
      (BitMatrix:getRowLoop1 row rowSize bits offset 0)))
  
  #|getTopLeftonBitLoops
getTopLeftOnBit helper function
|#
  (defun BitMatrix:getTopLeftOnBitLoop2 (theBits bit)
    (if (equal (<< theBits (- 31 bit)) 0)
        (BitMatrix:getTopLeftOnBitLoop2 theBits (+ bit 1))
        bit))
  
  #|getTopLeftonBitLoops
getTopLeftOnBit helper function
|#
  (defun BitMatrix:getTopLeftOnBitLoop1 (bits bitsOffset)
    (if (and (< bitsOffset (length bits)) (equal (nth bitsOffset bits) 0))
        (BitMatrix:getTopLeftOnBitLoop1 bits (+ bitsOffset 1))
        bitsOffset))
  
  #|BitMatrix:getTopLeftOnBit
bm:list
return:list
gets the top left information of the matrix
|#
  (defun BitMatrix:getTopLeftOnBit (bm)
    (let* ((bits (nth *BitMatrix:BITS* bm))
           (bitsOffset (BitMatrix:getTopLeftOnBitLoop1 bits 0)))
      (if (equal bitsOffset (length bits))
          nil
          (let* ((rowSize (nth *BitMatrix:ROW_SIZE* bm))
                 (y (truncate bitsOffset rowSize))
                 (x (<< (mod bitsOffset rowSize) 5))
                 (theBits (nth bitsOffset bits))
                 (bit (BitMatrix:getTopLeftOnBitLoop2 theBits 0)))
            (list (+ x bit) y)))))
  
  #|BitMatrix:getWidth(bm)
bm:list
return:int
gets the width of the matrix
|#
  (defun BitMatrix:getWidth (bm)
    (nth *BitMatrix:WIDTH* bm))
  
  #|BitMatrix:getHeight(bm) 
bm:list
return:int
gets the height value of the matrix
|#
  (defun BitMatrix:getHeight (bm)
    (nth *BitMatrix:HEIGHT* bm))
  
  #|ByteMatrix$2(width height)
width:int
height:int
return:list
ByteMatrix constructor takes two arguments
|#
  (defun ByteMatrix$2 (width height)
    (let* ((bytes (createList height (createList width 0))))
      ; (let* ((bytes (Matrix:set nil (- height 1) (- width 1) 0)))
      (list "ByteMatrix" bytes width height)))
  
  (defconst *ByteMatrix:BYTES* 1)
  (defconst *ByteMatrix:WIDTH* 2)
  (defconst *ByteMatrix:HEIGHT* 3)
  
  (defconst *ByteMatrix:QUIET_ZONE_SIZE* 4)
  
  #|ByteMatrix:getHeight
bm:list
return:int
get height of the ByteMatrix
|#
  (defun ByteMatrix:getHeight (Bm)
    (nth *ByteMatrix:HEIGHT* Bm))
  
  #|ByteMatrix:getWidth
bm:list
return:int
gets the width of the ByteMatrix
|#
  (defun ByteMatrix:getWidth (Bm)
    (nth *ByteMatrix:WIDTH* Bm))
  
  #|ByteMatrix:get(bm x y)
bm:list
x:int
y:int
return:int
get a specific element in the matrix
|#
  (defun ByteMatrix:get (Bm x y)
    (let* ((bytes (nth *ByteMatrix:BYTES* Bm))
           (row (nth y bytes)))
      (nth x row)))
  ; (Matrix:get (nth *ByteMatrix:BYTES* Bm) y x))
  
  #|ByteMatrix:getArray(bm)
bm:list
return:int
get an arrays in the ByteMatrix
|#
  (defun ByteMatrix:getArray (Bm)
    (nth *ByteMatrix:BYTES* Bm))
  ;  (Matrix:flatten (nth *ByteMatrix:BYTES* Bm)))
  
  #|ByteMatrix:set(bm x y v)
bm:list
x:int
y:int
v:int
return:list
Sets a value in the ByteMatrix
|#
  (defun ByteMatrix:set (Bm x y v)
    (let* ((value (if (booleanp v)
                      (if v 1 0)
                      v))
           (bytes (nth *ByteMatrix:BYTES* Bm))
           (row (nth y bytes))
           (newrow (put-nth x value row))
           (newbytes (put-nth y newrow bytes)))
      ;(newbytes (Matrix:set bytes y x value)))
      (put-nth *ByteMatrix:BYTES* newbytes Bm)))
  
  #|ByteMatrix:clear(bm value)
bm:list
value:int
return:list
clear the ByteMatrix
|#
  (defun ByteMatrix:clear (Bm value)
    (let* ((width (nth *ByteMatrix:WIDTH* Bm))
           (height (nth *ByteMatrix:HEIGHT* Bm))
           (matrix (nth *ByteMatrix:BYTES* Bm))
           (bytes (createList height (createList width value))))
      ; (bytes (Matrix:clear matrix value)))
      (put-nth *ByteMatrix:BYTES* bytes Bm)))
  
  (export IMatrix))