;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
(require "IEncoder.lisp")
(require "IMatrix.lisp")
(require "IUtilities.lisp")
(require "IQRCode.lisp")
(module MEncoder
  (include-book "list-utilities" :dir :teachpacks)
  (include-book "io-utilities" :dir :teachpacks)
  (import IMatrix)
  (import IUtilities)
  (import IQRCode)
  
  (defconst *Mode:BYTE* (Mode (list 8 16 16) 4 "BYTE"))
  (defconst *Mode:NUMERIC* (Mode (list 10 12 14) 1 "NUMERIC"))
  (defconst *Mode:ALPHANUMERIC* (Mode (list 9 11 13) 2 "ALPHANUMERIC"))
  (defconst *QRCODE:NUM_MASK_PATTERNS* 8)
  
  #|MaskUtil:applyMaskPenaltyRule1InternalLoop2(isHorizontal penalty numSameBitCells prevBit jLimit array i j)
isHorizontal: boolean
penalty: number
numSameBitCells: number
prevBit: number
jLimit: number
array: list
i: number
j: number
return (list penalty prevBit)
Second Helper Function for MaskUtil:applyMaskPenaltyRule1Internal
|#
  (defun MaskUtil:applyMaskPenaltyRule1InternalLoop2 
    (isHorizontal penalty numSameBitCells prevBit jLimit array i j)
    (if (< j jLimit)
        (let* ((bit (if isHorizontal 
                        (nth j (nth i array)) 
                        (nth i (nth j array)))))
          (if (equal bit prevBit)
              (let* ((newnumSameBitCells (+ numSameBitCells 1))
                     (newpenalty (if (equal newnumSameBitCells 5)
                                     (+ penalty 3)
                                     (if (> newnumSameBitCells 5)
                                         (+ penalty 1)
                                         penalty))))
                (MaskUtil:applyMaskPenaltyRule1InternalLoop2 
                 isHorizontal newpenalty newnumSameBitCells 
                 prevBit jLimit array i (+ j 1)))
              (let* ((newnumSameBitCells 1)
                     (newprevBit bit))
                (MaskUtil:applyMaskPenaltyRule1InternalLoop2 
                 isHorizontal penalty newnumSameBitCells 
                 newprevBit jLimit array i (+ j 1)))))
        (list penalty prevBit)))
  
  #|MaskUtil:applyMaskPenaltyRule1InternalLoop1(isHorizontal penalty numSameBitCells prevBit iLimit jLimit array i)
isHorizontal:boolean
penalty: number
numSameBitCells: number
prevBit: number
iLimit: number
jLimit: number
array: list
iLimit: number
jLimit: number
array: list
i: number
return penalty(number)
First Helper Function for MaskUtil:applyMaskPenaltyRule1Internal
|#
  (defun MaskUtil:applyMaskPenaltyRule1InternalLoop1 
    (isHorizontal penalty numSameBitCells prevBit iLimit jLimit array i)
    (if (< i iLimit)
        (let* ((j 0)
               (newvalues (MaskUtil:applyMaskPenaltyRule1InternalLoop2 
                           isHorizontal penalty numSameBitCells 
                           prevBit jLimit array i j))
               (newpenalty (nth 0 newvalues))
               (newprevBit (nth 1 newvalues)))
          (MaskUtil:applyMaskPenaltyRule1InternalLoop1
           isHorizontal newpenalty numSameBitCells 
           newprevBit iLimit jLimit array (+ i 1)))
        penalty))
  
  #|MaskUtil:applyMaskPenaltyRule1Internal(Bm isHorizontal)
Bm: ByteMatrix
isHorizontal: boolean
return penalty(number)
Calculates the penalty for rule 1
|#
  (defun MaskUtil:applyMaskPenaltyRule1Internal (Bm isHorizontal)
    (let* ((numSameBitCells 0)
           (penalty 0)
           (prevBit -1)
           (width (ByteMatrix:getWidth Bm))
           (height (ByteMatrix:getHeight Bm))
           (iLimit (if isHorizontal height width))
           (jLimit (if isHorizontal width height))
           (array (ByteMatrix:getArray Bm))
           (i 0))
      (MaskUtil:applyMaskPenaltyRule1InternalLoop1
       isHorizontal penalty numSameBitCells
       prevBit iLimit jLimit array i)))
  
  #|MaskUtil:applyMaskPenaltyRule1(Bm)
Bm: ByteMatrix
return penalty(number)
Calculates the penalty for rule 1
|#
  (defun MaskUtil:applyMaskPenaltyRule1 (Bm)
    (+ (MaskUtil:applyMaskPenaltyRule1Internal Bm 't)
       (MaskUtil:applyMaskPenaltyRule1Internal Bm nil)))
  
  #|MaskUtil:applyMaskPenaltyRule2Loop2(penalty array width height y x)
penalty: number
array: list
width: number
height: number
y: number
x: number
return penalty(number)
Second Helper function for MaskUtil:applyMaskPenaltyRule2
|#
  (defun MaskUtil:applyMaskPenaltyRule2Loop2 
    (penalty array width height y x)
    (if (< x (- width 1))
        (let* ((value (nth x (nth y array))))
          (if (and (equal value (nth (+ x 1) (nth y array)))
                   (equal value (nth x (nth (+ y 1) array)))
                   (equal value (nth (+ x 1) (nth (+ y 1) array))))
              (MaskUtil:applyMaskPenaltyRule2Loop2 
               (+ penalty 3) array width height y (+ x 1))
              (MaskUtil:applyMaskPenaltyRule2Loop2 
               penalty array width height y (+ x 1))))
        penalty))
  
  #|MaskUtil:applyMaskPenaltyRule2Loop1(penalty array width height y)
penalty: number
array: list
width: number
height: number
y: number
return penalty(number)
First Helper Function for MaskUtil:applyMaskPenaltyRule2
|#
  (defun MaskUtil:applyMaskPenaltyRule2Loop1 (penalty array width height y)
    (if (< y (- height 1))
        (let* ((newpenalty 
                (MaskUtil:applyMaskPenaltyRule2Loop2 
                 penalty array width height y 0)))
          (MaskUtil:applyMaskPenaltyRule2Loop1 
           newpenalty array width height (+ y 1)))
        penalty))
  
  #|MaskUtil:applyMaskPenaltyRule2(Bm)
Bm: ByteMatrix
return penalty(number)
Calculates the penalty for rule 2
|#
  (defun MaskUtil:applyMaskPenaltyRule2 (Bm)
    (let* ((array (ByteMatrix:getArray Bm))
           (width (ByteMatrix:getWidth Bm))
           (height (ByteMatrix:getHeight Bm)))
      (MaskUtil:applyMaskPenaltyRule2Loop1 0 array width height 0)))
  
  #|MaskUtil:applyMaskPenaltyRule3Loop2(penalty array width height y x)
penalty: number
array: list
width: number
height: number
y: number
x: number
return penalty(number)
Second Helper Function for MaskUtil:applyMaskPenaltyRule3
|#
  (defun MaskUtil:applyMaskPenaltyRule3Loop2 (penalty array width height y x)
    (if (< x width)
        (let* ((newpenalty1 
                (if (and (< (+ x 6) width)
                         (equal (nth x (nth y array)) 1)
                         (equal (nth (+ x 1) (nth y array)) 0)
                         (equal (nth (+ x 2) (nth y array)) 1)
                         (equal (nth (+ x 3) (nth y array)) 1)
                         (equal (nth (+ x 4) (nth y array)) 1)
                         (equal (nth (+ x 5) (nth y array)) 0)
                         (equal (nth (+ x 6) (nth y array)) 1)
                         (or (and (< (+ x 10) width)
                                  (equal (nth (+ x 7) (nth y array)) 0)
                                  (equal (nth (+ x 8) (nth y array)) 0)
                                  (equal (nth (+ x 9) (nth y array)) 0)
                                  (equal (nth (+ x 10) (nth y array)) 0))
                             (and (>= (- x 4) 0)
                                  (equal (nth (- x 1) (nth y array)) 0)
                                  (equal (nth (- x 2) (nth y array)) 0)
                                  (equal (nth (- x 3) (nth y array)) 0)
                                  (equal (nth (- x 4) (nth y array)) 0))))
                    (+ penalty 40)
                    penalty))
               (newpenalty2
                (if (and (< (+ y 6) height)
                         (equal (nth x (nth y array)) 1)
                         (equal (nth x (nth (+ y 1) array)) 0)
                         (equal (nth x (nth (+ y 2) array)) 1)
                         (equal (nth x (nth (+ y 3) array)) 1)
                         (equal (nth x (nth (+ y 4) array)) 1)
                         (equal (nth x (nth (+ y 5) array)) 0)
                         (equal (nth x (nth (+ y 6) array)) 1)
                         (or (and (< (+ y 10) height)
                                  (equal (nth x (nth (+ y 7) array)) 0)
                                  (equal (nth x (nth (+ y 8) array)) 0)
                                  (equal (nth x (nth (+ y 9) array)) 0)
                                  (equal (nth x (nth (+ y 10) array)) 0))
                             (and (>= (- y 4) 0)
                                  (equal (nth x (nth (- y 1) array)) 0)
                                  (equal (nth x (nth (- y 2) array)) 0)
                                  (equal (nth x (nth (- y 3) array)) 0)
                                  (equal (nth x (nth (- y 4) array)) 0))))
                    (+ newpenalty1 40)
                    newpenalty1)))
          (MaskUtil:applyMaskPenaltyRule3Loop2 
           newpenalty2 array width height y (+ x 1)))
        penalty))
  
  #|MaskUtil:applyMaskPenaltyRule3Loop1(penalty array width height y)
penalty: number
array: list
width: number
height: number
y: number
return penalty(number)
First Helper Function for MaskUtil:applyMaskPenaltyRule3
|#
  (defun MaskUtil:applyMaskPenaltyRule3Loop1 (penalty array width height y)
    (if (< y height)
        (let* ((newpenalty 
                (MaskUtil:applyMaskPenaltyRule3Loop2 
                 penalty array width height y 0)))
          (MaskUtil:applyMaskPenaltyRule3Loop1 
           newpenalty array width height (+ y 1)))
        penalty))
  
  #|MaskUtil:applyMaskPenaltyRule3(Bm)
Bm: ByteMatrix
return penalty(number)
Calculates the penalty for rule 3
|#
  (defun MaskUtil:applyMaskPenaltyRule3 (Bm)
    (let* ((array (ByteMatrix:getArray Bm))
           (width (ByteMatrix:getWidth Bm))
           (height (ByteMatrix:getHeight Bm)))
      (MaskUtil:applyMaskPenaltyRule3Loop1 0 array width height 0)))
  
  #|MaskUtil:applyMaskPenaltyRule4Loop2(numDarkCells array width height y x)
numDarkCells: number
array: list
width: number
height: number
y: number
x: number
return numDarkCells(number)
Second Helper function to Calculate the number of dark cells
|#
  (defun MaskUtil:applyMaskPenaltyRule4Loop2 
    (numDarkCells array width height y x)
    (if (< x width)
        (if (equal (nth x (nth y array)) 1)
            (MaskUtil:applyMaskPenaltyRule4Loop2 
             (+ numDarkCells 1) array width height y (+ x 1))
            (MaskUtil:applyMaskPenaltyRule4Loop2 
             numDarkCells array width height y (+ x 1)))
        numDarkCells))
  
  #|MaskUtil:applyMaskPenaltyRule4Loop1(numDarkCells array width height y)
numDarkCells: number
array: list
width: number
height: number
y: number
return numDarkCalls(number)
First Helper function to calculate the number of dark cells
|#
  (defun MaskUtil:applyMaskPenaltyRule4Loop1 
    (numDarkCells array width height y)
    (if (< y height)
        (let* ((newnumDarkCells 
                (MaskUtil:applyMaskPenaltyRule4Loop2 
                 numDarkCells array width height y 0)))
          (MaskUtil:applyMaskPenaltyRule4Loop1 
           newnumDarkCells array width height (+ y 1)))
        numDarkCells))
  
  #|MaskUtil:applyMaskPenaltyRule4(Bm)
Bm: ByteMatrix
return penalty(number)
Calculates the penalty for rule 4
|#
  (defun MaskUtil:applyMaskPenaltyRule4 (Bm)
    (let* ((array (ByteMatrix:getArray Bm))
           (width (ByteMatrix:getWidth Bm))
           (height (ByteMatrix:getHeight Bm))
           (numDarkCells 
            (MaskUtil:applyMaskPenaltyRule4Loop1 0 array width height 0))
           (numTotalCells (* height width))
           (darkRatio (/ numDarkCells numTotalCells)))
      (* (truncate (abs (truncate (- (* darkRatio 100) 50) 1)) 5) 10)))
  
  #|MaskUtil:getDataMaskBit(maskPattern x y)
maskPattern: number
x: number
y: number
return maskBit(number)
Returns the mask bit for "getMaskPattern" at "x" and "y"
|#
  (defun MaskUtil:getDataMaskBit (maskPattern x y)
    (if (equal maskPattern 0)
        (equal (logand (+ y x) 1) 0)
        (if (equal maskPattern 1)
            (equal (logand y 1) 0)
            (if (equal maskPattern 2)
                (equal (mod x 3) 0)
                (if (equal maskPattern 3)
                    (equal (mod (+ y x) 3) 0)
                    (if (equal maskPattern 4)
                        (equal (logand (+ (>>> y 1) (truncate x 3)) 1) 0)
                        (if (equal maskPattern 5)
                            (equal (+ (logand (* y x) 1) (mod (* y x) 3)) 0)
                            (if (equal maskPattern 6)
                                (equal (logand (+ (logand (* y x) 1) 
                                                  (mod (* y x) 3)) 1) 0)
                                (if (equal maskPattern 7)
                                    (equal (logand (+ (mod (* y x) 3) 
                                                      (logand (+ y x) 1)) 
                                                   1) 0)
                                    nil)))))))))
  (defconst *MatrixUtil:POSITION_DETECTION_PATTERN*
    (list (list 1 1 1 1 1 1 1)
          (list 1 0 0 0 0 0 1)
          (list 1 0 1 1 1 0 1)
          (list 1 0 1 1 1 0 1)
          (list 1 0 1 1 1 0 1)
          (list 1 0 0 0 0 0 1)
          (list 1 1 1 1 1 1 1)))
  
  (defconst *MatrixUtil:HORIZONTAL_SEPARATION_PATTERN*
    (list (list 0 0 0 0 0 0 0 0)))
  
  (defconst *MatrixUtil:VERTICAL_SEPARATION_PATTERN*
    (list (list 0) (list 0) (list 0) (list 0) (list 0) (list 0) (list 0)))
  
  (defconst *MatrixUtil:POSITION_ADJUSTMENT_PATTERN*
    (list (list 1 1 1 1 1)
          (list 1 0 0 0 1)
          (list 1 0 1 0 1)
          (list 1 0 0 0 1)
          (list 1 1 1 1 1)))
  
  (defconst *MatrixUtil:POSITION_ADJUSTMENT_PATTERN_COORDINATE_TABLE*
    (list (list -1 -1 -1 -1  -1  -1  -1)  
          (list 6 18 -1 -1  -1  -1  -1)  
          (list 6 22 -1 -1  -1  -1  -1)  
          (list 6 26 -1 -1  -1  -1  -1)  
          (list 6 30 -1 -1  -1  -1  -1)  
          (list 6 34 -1 -1  -1  -1  -1)  
          (list 6 22 38 -1  -1  -1  -1)  
          (list 6 24 42 -1  -1  -1  -1)  
          (list 6 26 46 -1  -1  -1  -1)  
          (list 6 28 50 -1  -1  -1  -1)  
          (list 6 30 54 -1  -1  -1  -1)  
          (list 6 32 58 -1  -1  -1  -1)  
          (list 6 34 62 -1  -1  -1  -1)  
          (list 6 26 46 66  -1  -1  -1)  
          (list 6 26 48 70  -1  -1  -1)  
          (list 6 26 50 74  -1  -1  -1)  
          (list 6 30 54 78  -1  -1  -1)  
          (list 6 30 56 82  -1  -1  -1)  
          (list 6 30 58 86  -1  -1  -1)  
          (list 6 34 62 90  -1  -1  -1)  
          (list 6 28 50 72  94  -1  -1)  
          (list 6 26 50 74  98  -1  -1)  
          (list 6 30 54 78 102  -1  -1)  
          (list 6 28 54 80 106  -1  -1)  
          (list 6 32 58 84 110  -1  -1)  
          (list 6 30 58 86 114  -1  -1)  
          (list 6 34 62 90 118  -1  -1)  
          (list 6 26 50 74  98 122  -1)  
          (list 6 30 54 78 102 126  -1)  
          (list 6 26 52 78 104 130  -1)  
          (list 6 30 56 82 108 134  -1)  
          (list 6 34 60 86 112 138  -1)  
          (list 6 30 58 86 114 142  -1)  
          (list 6 34 62 90 118 146  -1)  
          (list 6 30 54 78 102 126 150)  
          (list 6 24 50 76 102 128 154)  
          (list 6 28 54 80 106 132 158)  
          (list 6 32 58 84 110 136 162)  
          (list 6 26 54 82 110 138 166)  
          (list 6 30 58 86 114 142 170)))
  
  (defconst *MatrixUtil:TYPE_INFO_COORDINATES*
    (list (list 8 0)
          (list 8 1)
          (list 8 2)
          (list 8 3)
          (list 8 4)
          (list 8 5)
          (list 8 7)
          (list 8 8)
          (list 7 8)
          (list 5 8)
          (list 4 8)
          (list 3 8)
          (list 2 8)
          (list 1 8)
          (list 0 8)))
  
  (defconst *MatrixUtil:VERSION_INFO_POLY* 7973)
  (defconst *MatrixUtil:TYPE_INFO_POLY* 1335)
  (defconst *MatrixUtil:TYPE_INFO_MASK_PATTERN* 21522)
  
  #|MatrixUtil:clearMatrix(Bm)
Bm: ByteMatrix
returns a Byte Matrix
Clears the bytematrix and sets all of its values to -1
|#
  (defun MatrixUtil:clearMatrix (Bm)
    (ByteMatrix:clear Bm -1))
  
  #|MatrixUtil:isEmpty(value)
value: number
returns boolean
checks to see if the value is equal to -1
|#
  (defun MatrixUtil:isEmpty (value)
    (equal value -1))
  
  #|MatrixUtil:isValidValue(value)
value: number
returns boolean
checks to make sure the value is valid for the ByteMatrix
|#
  (defun MatrixUtil:isValidValue (value)
    (or (equal value -1) (equal value 0) (equal value 1)))
  
  #|MatrixUtil:embedDataBitsLoop3(dataBits maskPattern Bm bitIndex direction x y i)
dataBits: bitArray
maskPattern: number
Bm: ByteMatrix
bitIndex: number
direction: number
x: number
y: number
i: number
returns (list Bm bitIndex)
third helper function to embed the data encoded from the string into the matrix
|#
  (defun MatrixUtil:embedDataBitsLoop3 
    (dataBits maskPattern Bm bitIndex direction x y i)
    (if (< i 2)
        (let* ((xx (- x i)))
          (if (not (MatrixUtil:isEmpty (ByteMatrix:get Bm xx y)))
              (MatrixUtil:embedDataBitsLoop3 
               dataBits maskPattern Bm bitIndex direction x y (+ i 1))
              (let* ((bit (if (< bitIndex (BitArray:getSize dataBits))
                              (BitArray:get dataBits bitIndex)
                              nil))
                     (newbitindex (if (< bitIndex (BitArray:getSize dataBits))
                                      (+ bitIndex 1)
                                      bitIndex))
                     (finalbit (if (and 
                                    (not (equal maskPattern -1))
                                    (MaskUtil:getDataMaskBit maskPattern xx y))
                                   (not bit)
                                   bit))
                     (matrix (ByteMatrix:set Bm xx y finalbit)))
                (MatrixUtil:embedDataBitsLoop3 
                 dataBits maskPattern matrix 
                 newbitindex direction x y (+ i 1)))))
        (list Bm bitIndex)))
  
  #|MatrixUtil:embedDataBitsLoop2(dataBits maskPattern Bm bitIndex direction x y)
dataBits: bitArray
maskPattern: number
Bm: ByteMatrix
bitIndex: number
direction: number
x: number
y: number
returns (list Bm bitIndex y)
second helper function to embed the data encoded from the string into the matrix
|#
  (defun MatrixUtil:embedDataBitsLoop2 
    (dataBits maskPattern Bm bitIndex direction x y)
    (if (and (>= y 0) (< y (ByteMatrix:getHeight Bm)))
        (let* ((i 0)
               (newvalues (MatrixUtil:embedDataBitsLoop3 
                           dataBits maskPattern Bm bitIndex direction x y i))
               (matrix (nth 0 newvalues))
               (newbitIndex (nth 1 newvalues)))
          (MatrixUtil:embedDataBitsLoop2 
           dataBits maskPattern matrix 
           newbitIndex direction x (+ y direction)))
        (list Bm bitIndex y)))
  
  #|MatrixUtil:embedDataBitsLoop1(dataBits maskPattern Bm bitIndex direction x y)
dataBits: bitArray
maskPattern: number
Bm: ByteMatrix
bitIndex: number
direction: number
x: number
y: number
returns Bm (ByteMatrix)
first helper function to embed the data encoded from the string into the matrix
|#
  (defun MatrixUtil:embedDataBitsLoop1 
    (dataBits maskPattern Bm bitIndex direction x y)
    (if (> x 0)
        (let* ((newx (if (equal x 6) (- x 1) x))
               (newvalues (MatrixUtil:embedDataBitsLoop2
                           dataBits maskPattern Bm 
                           bitIndex direction newx y))
               (matrix (nth 0 newvalues))
               (newbitIndex (nth 1 newvalues))
               (newy (nth 2 newvalues)))
          (MatrixUtil:embedDataBitsLoop1 
           dataBits maskPattern matrix 
           newbitIndex (* -1 direction) (- newx 2) (+ newy (* -1 direction))))
        Bm))
  
  #|MatrixUtil:embedDataBits(dataBits maskPattern Bm)
dataBits: bitArray
maskPattern: number
Bm: ByteMatrix
returns Bm (ByteMatrix)
embeds the data encoded from the string into the matrix
|#
  (defun MatrixUtil:embedDataBits (dataBits maskPattern Bm)
    (let* ((bitIndex 0)
           (direction -1)
           (x (- (ByteMatrix:getWidth Bm) 1))
           (y (- (ByteMatrix:getHeight Bm) 1)))
      (MatrixUtil:embedDataBitsLoop1
       dataBits maskPattern Bm bitIndex direction x y)))
  
  #|MatrixUtil:findMSBSetLoop1 (value numDigits)
value: number
numDigits: number
return numDigits(number)
helper function for findMSBSet
|#
  (defun MatrixUtil:findMSBSetLoop1 (value numDigits)
    (if (not (equal value 0))
        (MatrixUtil:findMSBSetLoop1 (>>> value 1) (+ numDigits 1))
        numDigits))
  
  #|MatrixUtil:findMSBSet(value)
value: number
Return the position of the most significant bit set (to one) in the "value".
The most significant bit is position 32. If there is no bit set, return 0.
|#
  (defun MatrixUtil:findMSBSet (value)
    (let* ((numDigits 0))
      (MatrixUtil:findMSBSetLoop1 value numDigits)))
  
  #|MatrixUtil:calculateBCHCodeLoop1 (value poly msbSetInPoly)
value: number
poly: number
msbSetInPoly: number
return value(number)
helper function for MatrixUtil:calculateBCHCode
|#
  (defun MatrixUtil:calculateBCHCodeLoop1 (value poly msbSetInPoly)
    (if (>= (MatrixUtil:findMSBSet value) msbSetInPoly)
        (let* ((newvalue 
                (<< poly (- (MatrixUtil:findMSBSet value) msbSetInPoly))))
          (MatrixUtil:calculateBCHCodeLoop1 
           (logxor value newvalue) poly msbSetInPoly))
        value))
  
  #|MatrixUtil:calculateBCHCode(value poly)
value:number
poly:number
return value(number)
Calculates BCH (Bose-Chaudhuri-Hocquenghem) code for "value" using polynomial
"poly". The BCH code is used for encoding type information and version information.
|#
  (defun MatrixUtil:calculateBCHCode (value poly)
    (let* ((msbSetInPoly (MatrixUtil:findMSBSet poly))
           (newvalue (<< value (- msbSetInPoly 1))))
      (MatrixUtil:calculateBCHCodeLoop1 newvalue poly msbSetInPoly)))
  
  #|MatrixUtil:makeTypeInfoBits (ecLevel maskPattern bits)
ecLevel: ErrorCorrectionLevel
maskPattern: number
bits: number
|#
  (defun MatrixUtil:makeTypeInfoBits (ecLevel maskPattern bits)
    (let* ((typeInfo 
            (logior (<< (ErrorCorrectionLevel:getBits ecLevel) 3) maskPattern))
           (bitsType (BitArray:appendBits bits typeInfo 5))
           (bchCode 
            (MatrixUtil:calculateBCHCode typeInfo *MatrixUtil:TYPE_INFO_POLY*))
           (bitsCode (BitArray:appendBits bitsType bchCode 10))
           (maskBits 
            (BitArray:appendBits 
             (BitArray) *MatrixUtil:TYPE_INFO_MASK_PATTERN* 15)))
      (BitArray:xor bitsCode maskBits)))
  
  #|MatrixUtil:embedTypeInfoLoop1 (Bm typeInfoBits i)
Bm: ByteMatrix
typeInfoBits: BitArray
i: number
return a modified Bm with the type information embedded
|#
  (defun MatrixUtil:embedTypeInfoLoop1 (Bm typeInfoBits i)
    (if (< i (BitArray:getSize typeInfoBits))
        (let* ((bit (BitArray:get 
                     typeInfoBits 
                     (+ (BitArray:getSize typeInfoBits) -1 (* -1 i))))
               (x1 (nth 0 (nth i *MatrixUtil:TYPE_INFO_COORDINATES*)))
               (y1 (nth 1 (nth i *MatrixUtil:TYPE_INFO_COORDINATES*)))
               (matrix1 (ByteMatrix:set Bm x1 y1 bit)))
          (if (< i 8)
              (let* ((x2 (+ (ByteMatrix:getWidth matrix1) (* -1 i) -1))
                     (y2 8)
                     (matrix2 (ByteMatrix:set matrix1 x2 y2 bit)))
                (MatrixUtil:embedTypeInfoLoop1 matrix2 typeInfoBits (+ i 1)))
              (let* ((x2 8)
                     (y2 (+ (ByteMatrix:getHeight matrix1) -7 (- i 8)))
                     (matrix2 (ByteMatrix:set matrix1 x2 y2 bit)))
                (MatrixUtil:embedTypeInfoLoop1 matrix2 typeInfoBits (+ i 1)))))
        Bm))
  
  #|MatrixUtil:embedTypeInfo (ecLevel maskPattern Bm)
ecLevel: ErrorCorrectionLevel
maskPattern: number
Bm: ByteMatrix
return a modified Bm with the type information embedded
|#
  (defun MatrixUtil:embedTypeInfo (ecLevel maskPattern Bm)
    (let* ((typeInfoBits 
            (MatrixUtil:makeTypeInfoBits ecLevel maskPattern (BitArray))))
      (MatrixUtil:embedTypeInfoLoop1 Bm typeInfoBits 0)))
  
  #|MatrixUtil:makeVersionInfoBits (version bits)
version: number
bits: BitArray
returns a bitArray with the version info bits encoded
|#
  (defun MatrixUtil:makeVersionInfoBits (version bits)
    (let* ((versionBits (BitArray:appendBits bits version 6))
           (bchCode 
            (MatrixUtil:calculateBCHCode 
             version *MatrixUtil:VERSION_INFO_POLY*)))
      (BitArray:appendBits versionBits bchCode 12)))
  
  #|MatrixUtil:maybeEmbedVersionInfoLoop2 (Bm versionInfoBits bitIndex i j)
Bm:ByteMatrix
versionInfoBits:bitArray
bitIndex:number
i:number
j:number
returns a modified Bm with the version information embedded
|#
  (defun MatrixUtil:maybeEmbedVersionInfoLoop2 
    (Bm versionInfoBits bitIndex i j)
    (if (< j 3)
        (let* ((bit (BitArray:get versionInfoBits bitIndex))
               (bottomLeft 
                (ByteMatrix:set Bm i (+ (ByteMatrix:getHeight Bm) -11 j) bit))
               (bottomRight 
                (ByteMatrix:set 
                 bottomLeft (+ (ByteMatrix:getHeight bottomLeft) -11 j) i bit)))
          (MatrixUtil:maybeEmbedVersionInfoLoop2 
           bottomRight versionInfoBits (- bitIndex 1) i (+ j 1)))
        Bm))
  
  #|MatrixUtil:maybeEmbedVersionInfoLoop1 (Bm versionInfoBits bitIndex i)
Bm:ByteMatrix
versionInfoBits:bitArray
bitIndex:number
i:number
returns a modified Bm with the version information embedded
|#
  (defun MatrixUtil:maybeEmbedVersionInfoLoop1 (Bm versionInfoBits bitIndex i)
    (if (< i 6)
        (let* ((j 0)
               (matrix 
                (MatrixUtil:maybeEmbedVersionInfoLoop2 
                 Bm versionInfoBits bitIndex i j)))
          (MatrixUtil:maybeEmbedVersionInfoLoop1 
           matrix versionInfoBits (- bitIndex 3) (+ i 1)))
        Bm))
  
  #|MatrixUtil:maybeEmbedVersionInfo (version Bm)
version: number
Bm: ByteMatrix
returns a modified Bm with the version information embedded
|#
  (defun MatrixUtil:maybeEmbedVersionInfo (version Bm)
    (if (< version 7)
        Bm
        (let* ((versionInfoBits 
                (MatrixUtil:makeVersionInfoBits version (BitArray)))
               (bitIndex 17)
               (i 0))
          (MatrixUtil:maybeEmbedVersionInfoLoop1 
           Bm versionInfoBits bitIndex i))))
  
  #|MatrixUtil:embedTimingPatternsLoop1 (Bm i width)
Bm:ByteMatrix
i:number
width:number
returns a modified Bm with the timing information embedded
|#
  (defun MatrixUtil:embedTimingPatternsLoop1 (Bm i width)
    (if (< i (- width 8))
        (let* ((bit (mod (+ i 1) 2))
               (matrix1 (if (MatrixUtil:isEmpty (ByteMatrix:get Bm i 6))
                            (ByteMatrix:set Bm i 6 bit)
                            Bm))
               (matrix2 (if (MatrixUtil:isEmpty (ByteMatrix:get matrix1 6 i))
                            (ByteMatrix:set matrix1 6 i bit)
                            matrix1)))
          (MatrixUtil:embedTimingPatternsLoop1 matrix2 (+ i 1) width))
        Bm))
  
  #|MatrixUtil:embedTimingPatternsLoop1 (Bm i width)
Bm:ByteMatrix
returns a modified Bm with the timing information embedded
|#
  (defun MatrixUtil:embedTimingPatterns (Bm)
    (let* ((i 8)
           (width (ByteMatrix:getWidth Bm)))
      (MatrixUtil:embedTimingPatternsLoop1 Bm i width)))
  
  #|MatrixUtil:embedDarkDotAtLeftBottomCorner (Bm)
Bm:ByteMatrix
returns a modified Bm with a dark dot in the bottom left corner
|#
  (defun MatrixUtil:embedDarkDotAtLeftBottomCorner (Bm)
    (ByteMatrix:set Bm 8 (- (ByteMatrix:getHeight Bm) 8) 1))
  
  #|MatrixUtil:embedHorizontalSeperationPatternLoop1 (xStart yStart Bm x)
xStart:number
yStart:number
Bm:ByteMatrix
x:number
returns a modified Bm with the horizontal seperation pattern embedded
|#
  (defun MatrixUtil:embedHorizontalSeperationPatternLoop1 
    (xStart yStart Bm x)
    (if (< x 8)
        (let* ((matrix 
                (ByteMatrix:set 
                 Bm (+ xStart x) yStart 
                 (nth x (nth 0 *MatrixUtil:HORIZONTAL_SEPARATION_PATTERN*)))))
          (MatrixUtil:embedHorizontalSeperationPatternLoop1 
           xStart yStart matrix (+ x 1)))
        Bm))
  
  #|MatrixUtil:embedHorizontalSeperationPattern (xStart yStart Bm)
xStart:number
yStart:number
Bm:ByteMatrix
returns a modified Bm with the horizontal seperation pattern embedded
|#  
  (defun MatrixUtil:embedHorizontalSeperationPattern (xStart yStart Bm)
    (let* ((x 0))
      (MatrixUtil:embedHorizontalSeperationPatternLoop1 xStart yStart Bm x)))
  
  #|MatrixUtil:embedVerticalSeperationPatternLoop1 (xStart yStart Bm y)
xStart:number
yStart:number
Bm:ByteMatrix
y:number
returns a modified Bm with the vertical seperation pattern embedded
|#
  (defun MatrixUtil:embedVerticalSeperationPatternLoop1 (xStart yStart Bm y)
    (if (< y 7)
        (let* ((matrix 
                (ByteMatrix:set 
                 Bm xStart (+ yStart y) 
                 (nth 0 (nth y *MatrixUtil:VERTICAL_SEPARATION_PATTERN*)))))
          (MatrixUtil:embedVerticalSeperationPatternLoop1 
           xStart yStart matrix (+ y 1)))
        Bm))
  
  #|MatrixUtil:embedVerticalSeperationPattern(xStart yStart Bm)
xStart:number
yStart:number
Bm:ByteMatrix
returns a modified Bm with the vertical seperation pattern embedded
|#
  (defun MatrixUtil:embedVerticalSeperationPattern (xStart yStart Bm)
    (let* ((y 0))
      (MatrixUtil:embedVerticalSeperationPatternLoop1 xStart yStart Bm y)))
  
  
  #|MatrixUtil:embedPositionAdjustmentPatternLoop2 (xStart yStart Bm y x)
xStart:number
yStart:number
Bm:ByteMatrix
y:number
x:number
returns a modified Bm with the position adjustment pattern embedded
|#
  (defun MatrixUtil:embedPositionAdjustmentPatternLoop2 (xStart yStart Bm y x)
    (if (< x 5)
        (let* ((matrix 
                (ByteMatrix:set 
                 Bm (+ xStart x) (+ yStart y) 
                 (nth x (nth y *MatrixUtil:POSITION_ADJUSTMENT_PATTERN*)))))
          (MatrixUtil:embedPositionAdjustmentPatternLoop2 
           xStart yStart matrix y (+ x 1)))
        Bm))
  
  #|MatrixUtil:embedPositionAdjustmentPatternLoop1 (xStart yStart Bm y)
xStart:number
yStart:number
Bm:ByteMatrix
y:number
returns a modified Bm with the position adjustment pattern embedded
|#
  (defun MatrixUtil:embedPositionAdjustmentPatternLoop1 (xStart yStart Bm y)
    (if (< y 5)
        (let* ((x 0)
               (matrix 
                (MatrixUtil:embedPositionAdjustmentPatternLoop2
                 xStart yStart Bm y x)))
          (MatrixUtil:embedPositionAdjustmentPatternLoop1 
           xStart yStart matrix (+ y 1)))
        Bm))
  
  #|MatrixUtil:embedPositionAdjustmentPattern (xStart yStart Bm)
xStart:number
yStart:number
Bm:ByteMatrix
returns a modified Bm with the position adjustment pattern embedded
|#
  (defun MatrixUtil:embedPositionAdjustmentPattern (xStart yStart Bm)
    (let* ((y 0))
      (MatrixUtil:embedPositionAdjustmentPatternLoop1 xStart yStart Bm y)))
  
  
  #|MatrixUtil:embedPositionDetectionPatternLoop2 (xStart yStart Bm y x)
xStart:number
yStart:number
Bm:ByteMatrix
y:number
x:number
returns a modified Bm with the position detection pattern embedded
|#
  (defun MatrixUtil:embedPositionDetectionPatternLoop2 (xStart yStart Bm y x)
    (if (< x 7)
        (let* ((matrix 
                (ByteMatrix:set 
                 Bm (+ xStart x) (+ yStart y) 
                 (nth x (nth y *MatrixUtil:POSITION_DETECTION_PATTERN*)))))
          (MatrixUtil:embedPositionDetectionPatternLoop2 
           xStart yStart matrix y (+ x 1)))
        Bm))
  
  #|MatrixUtil:embedPositionDetectionPatternLoop1 (xStart yStart Bm y)
xStart:number
yStart:number
Bm:ByteMatrix
y:number
returns a modified Bm with the position detection pattern embedded
|#
  (defun MatrixUtil:embedPositionDetectionPatternLoop1 (xStart yStart Bm y)
    (if (< y 7)
        (let* ((x 0)
               (matrix 
                (MatrixUtil:embedPositionDetectionPatternLoop2
                 xStart yStart Bm y x)))
          (MatrixUtil:embedPositionDetectionPatternLoop1 
           xStart yStart matrix (+ y 1)))
        Bm))
  
  #|MatrixUtil:embedPositionDetectionPattern (xStart yStart Bm)
xStart:number
yStart:number
Bm:ByteMatrix
returns a modified Bm with the position detection pattern embedded
|#  
  (defun MatrixUtil:embedPositionDetectionPattern (xStart yStart Bm)
    (let* ((y 0))
      (MatrixUtil:embedPositionDetectionPatternLoop1 xStart yStart Bm y)))
  
  
  #|MatrixUtil:embedPositionDetectionPatternsAndSeparators (Bm)
Bm:ByteMatrix
returns a modified Bm with all of the position and seperator patterns embedded
|#
  (defun MatrixUtil:embedPositionDetectionPatternsAndSeparators (Bm)
    (let* ((pdpWidth (length (nth 0 *MatrixUtil:POSITION_DETECTION_PATTERN*)))
           (hspWidth (length (nth 0 *MatrixUtil:HORIZONTAL_SEPARATION_PATTERN*)))
           (vspSize (length *MatrixUtil:VERTICAL_SEPARATION_PATTERN*))
           (width (ByteMatrix:getWidth Bm))
           (height (ByteMatrix:getHeight Bm)))
      (MatrixUtil:embedVerticalSeperationPattern 
       vspSize (- height vspSize)
       (MatrixUtil:embedVerticalSeperationPattern 
        (+ height (* -1 vspSize) -1) 0
        (MatrixUtil:embedVerticalSeperationPattern 
         vspSize 0
         (MatrixUtil:embedHorizontalSeperationPattern 
          0 (- width hspWidth)
          (MatrixUtil:embedHorizontalSeperationpattern 
           (- width hspWidth) (- hspWidth 1)
           (MatrixUtil:embedHorizontalSeperationPattern 
            0 (- hspWidth 1)
            (MatrixUtil:embedPositionDetectionPattern 
             0 (- width pdpWidth)
             (MatrixUtil:embedPositionDetectionPattern 
              (- width pdpWidth) 0
              (MatrixUtil:embedPositionDetectionPattern 0 0 Bm)))))))))))
  
  
  #|MatrixUtil:maybeEmbedPositionAdjustmentPatternsLoop2(Bm coordinates numCoordinates i j)
Bm:ByteMatrix
coordinates:list
numCoordinates:number
i:number
j:number
returns a modified Bm with the position adjustment patterns embedded
|#
  (defun MatrixUtil:maybeEmbedPositionAdjustmentPatternsLoop2 
    (Bm coordinates numCoordinates i j)
    (if (< j numCoordinates)
        (let* ((y (nth i coordinates))
               (x (nth j coordinates)))
          (if (or (equal x -1) (equal y -1))
              (MatrixUtil:maybeEmbedPositionAdjustmentPatternsLoop2 
               Bm coordinates numCoordinates i (+ j 1))
              (if (MatrixUtil:isEmpty (ByteMatrix:get Bm x y))
                  (let* ((matrix 
                          (MatrixUtil:embedPositionAdjustmentPattern 
                           (- x 2) (- y 2) Bm)))
                    (MatrixUtil:maybeEmbedPositionAdjustmentPatternsLoop2 
                     matrix coordinates numCoordinates i (+ j 1)))
                  (MatrixUtil:maybeEmbedPositionAdjustmentPatternsLoop2
                   Bm coordinates numCoordinates i (+ j 1)))))
        Bm))
  
  #|MatrixUtil:maybeEmbedPositionAdjustmentPatternsLoop1(Bm coordinates numCoordinates i)
Bm:ByteMatrix
coordinates:list
numCoordinates:number
i:number
returns a modified Bm with the position adjustment patterns embedded
|#
  (defun MatrixUtil:maybeEmbedPositionAdjustmentPatternsLoop1 
    (Bm coordinates numCoordinates i)
    (if (< i numCoordinates)
        (let* ((j 0)
               (matrix (MatrixUtil:maybeEmbedPositionAdjustmentPatternsLoop2 
                        Bm coordinates numCoordinates i j)))
          (MatrixUtil:maybeEmbedPositionAdjustmentPatternsLoop1
           matrix coordinates numCoordinates (+ i 1)))
        Bm))
  
  #|MatrixUtil:maybeEmbedPositionAdjustmentPatterns(version Bm)
version: number
Bm:ByteMatrix
returns a modified Bm that might have the position adjustment patterns embedded
|#
  (defun MatrixUtil:maybeEmbedPositionAdjustmentPatterns (version Bm)
    (if (< version 2)
        Bm
        (let* ((index (- version 1))
               (coordinates 
                (nth 
                 index 
                 *MatrixUtil:POSITION_ADJUSTMENT_PATTERN_COORDINATE_TABLE*))
               (numCoordinates (length coordinates))
               (i 0))
          (MatrixUtil:maybeEmbedPositionAdjustmentPatternsLoop1 
           Bm coordinates numCoordinates i))))
  
  #|MatrixUtil:embedBasicPatterns (version Bm)
version:number
Bm:ByteMatrix
returns a modified Bm with position detection, timing, the dark dot, and position adjustment patterns embedded
|#
  (defun MatrixUtil:embedBasicPatterns (version Bm)
    (MatrixUtil:embedTimingPatterns
     (MatrixUtil:maybeEmbedPositionAdjustmentPatterns 
      version
      (MatrixUtil:embedDarkDotAtLeftBottomCorner 
       (MatrixUtil:embedPositionDetectionPatternsAndSeparators Bm)))))
  
  #| MatrixUtil:buildMatrix (dataBits ecLevel version maskPattern Bm)
dataBits:BitArray
ecLevel:ErrorCorrectionLevel
version:number
maskPattern:number
Bm:ByteMatrix
returns a modified Bm with the dataBits encoded with an error corection of ecLevel of version number "version" and with maskPattern "maskPattern"
|#
  (defun MatrixUtil:buildMatrix (dataBits ecLevel version maskPattern Bm)
    (MatrixUtil:embedDataBits 
     dataBits maskPattern 
     (MatrixUtil:maybeEmbedVersionInfo 
      version 
      (MatrixUtil:embedTypeInfo 
       ecLevel maskPattern 
       (MatrixUtil:embedBasicPatterns 
        version 
        (MatrixUtil:clearMatrix Bm))))))
  
  (defconst *GF256:QR_CODE_FIELD*
    '("GF256" (1 2 4 8 16 32 64 128 29 58 116 232 205 135 19 38 76 152 45 90
                 180 117 234 201 143 3 6 12 24 48 96 192 157 39 78 156 37 74
                 148 53 106 212 181 119 238 193 159 35 70 140 5 10 20 40 80 
                 160 93 186 105 210 185 111 222 161 95 190 97 194 153 47 94 
                 188 101 202 137 15 30 60 120 240 253 231 211 187 107 214 177 
                 127 254 225 223 163 91 182 113 226 217 175 67 134 17 34 68 
                 136 13 26 52 104 208 189 103 206 129 31 62 124 248 237 199 
                 147 59 118 236 197 151 51 102 204 133 23 46 92 184 109 218 
                 169 79 158 33 66 132 21 42 84 168 77 154 41 82 164 85 170 73
                 146 57 114 228 213 183 115 230 209 191 99 198 145 63 126 252
                 229 215 179 123 246 241 255 227 219 171 75 150 49 98 196 149
                 55 110 220 165 87 174 65 130 25 50 100 200 141 7 14 28 56 
                 112 224 221 167 83 166 81 162 89 178 121 242 249 239 195 155
                 43 86 172 69 138 9 18 36 72 144 61 122 244 245 247 243 251 
                 235 203 139 11 22 44 88 176 125 250 233 207 131 27 54 108 216
                 173 71 142 1)
              (0 0 1 25 2 50 26 198 3 223 51 238 27 104 199 75 4 100 224 14
                 52 141 239 129 28 193 105 248 200 8 76 113 5 138 101 47 225
                 36 15 33 53 147 142 218 240 18 130 69 29 181 194 125 106 39
                 249 185 201 154 9 120 77 228 114 166 6 191 139 98 102 221 48
                 253 226 152 37 179 16 145 34 136 54 208 148 206 143 150 219
                 189 241 210 19 92 131 56 70 64 30 66 182 163 195 72 126 110
                 107 58 40 84 250 133 186 61 202 94 155 159 10 21 121 43 78
                 212 229 172 115 243 167 87 7 112 192 247 140 128 99 13 103 
                 74 222 237 49 197 254 24 227 165 153 119 38 184 180 124 17 
                 68 146 217 35 32 137 46 55 63 209 91 149 188 207 205 144 135
                 151 178 220 252 190 97 242 86 211 171 20 42 93 158 132 60 57
                 83 71 109 65 162 31 45 67 216 183 123 164 118 196 23 73 236
                 127 12 111 246 108 161 59 82 41 157 85 170 251 96 134 177 
                 187 204 62 90 203 89 95 176 156 169 160 81 11 245 22 235 122
                 117 44 215 79 174 213 233 230 231 173 232 116 214 244 234 
                 168 80 88 175)))
  
  (defconst *GF256:EXP_Table* 1)
  (defconst *GF256:LOG_TABLE* 2)
  (defconst *GF256Poly:FIELD* 1)
  (defconst *GF256Poly:COEFFICIENTS* 2)
  
  #|GF256Poly$2Loop1 (coefficients firstNonZero)
coefficients:list
firstNonZero:number
returns firstNonZero(number)
helper function for GF256Poly$2
|#
  (defun GF256Poly$2Loop1 (coefficients firstNonZero)
    (if (and (< firstNonZero (length coefficients))
             (equal (nth firstNonZero coefficients) 0))
        (GF256Poly$2Loop1 coefficients (+ firstNonZero 1))
        firstNonZero))
  
  #|GF256Poly$2 (field coefficients)
field:GF256
coefficients: list
returns a GF256 polynomial
|#
  (defun GF256Poly$2 (field coefficients)
    (if (and (> (length coefficients) 1) (equal (nth 0 coefficients) 0))
        (let* ((firstNonZero (GF256Poly$2Loop1 coefficients 1))
               (finalcoefficients (if (equal firstNonZero (length coefficients))
                                      (nth *GF256Poly:COEFFICIENTS* (GF256Poly$2 field (list 0)))
                                      (nthcdr firstNonZero coefficients))))
          (list "GF256Poly" field finalcoefficients))
        (list "GF256Poly" field coefficients)))
  
  #|GF256:getZero (gf)
gf:GF256 field
returns the zero field
|#
  (defun GF256:getZero (gf)
    (GF256Poly$2 gf (list 0)))
  
  #|GF256:getOne (gf)
gf:GF256 field
returns the one field
|#
  (defun GF256:getOne (gf)
    (GF256Poly$2 gf (list 1)))
  
  #|GF256:buildMonomial (gf degree coefficient)
gf:GF256
degree:number
coefficient:number
returns a polynomial of degree "degree"
|#
  (defun GF256:buildMonomial (gf degree coefficient)
    (if (equal coefficient 0)
        (GF256:getZero gf)
        (GF256Poly$2 gf (put-nth 0 coefficient (createList (+ degree 1) 0)))))
  
  #|GF256:addOrSubtract (a b)
a:number
b:number
"adds" the polynomials together
|#
  (defun GF256:addOrSubtract (a b)
    (logxor a b))
  
  #|GF256:exp (gf a)
gf:GF256
a:number
returns the a exponent in the field from the exponent table
|#
  (defun GF256:exp (gf a)
    (nth a (nth *GF256:EXP_TABLE* gf)))
  
  #|GF256:log (gf a)
gf:GF256
a:number
returns the a logarithm in the field from the logarithm table
|#
  (defun GF256:log (gf a)
    (nth a (nth *GF256:LOG_TABLE* gf)))
  
  #|GF256:inverse (gf a)
gf:GF256
a:number
returns the inverse of the exponent of the log of a
|#
  (defun GF256:inverse (gf a)
    (GF256:exp gf (- 255 (GF256:log gf a))))
  
  #|GF256:multiply (gf a b)
gf:GF256
a:number
b:number
returns the multipled fields
|#
  (defun GF256:multiply (gf a b)
    (if (or (equal a 0) (equal b 0))
        0
        (let* ((logSum (+ (GF256:log gf a) (GF256:log gf b)))
               (index (+ (logand logSum 255) (>>> logSum 8))))
          (GF256:exp gf index))))
  
  #|GF256Poly:getCoefficients (gfp)
gfp:GF256Poly
returns the coefficients for the given polynomial
|#
  (defun GF256Poly:getCoefficients (gfp)
    (nth *GF256Poly:COEFFICIENTS* gfp))
  
  #|GF256Poly:getDegree (gfp)
gfp:GF256Poly
returns the degree of the given polynomial
|#
  (defun GF256Poly:getDegree (gfp)
    (- (length (nth *GF256Poly:COEFFICIENTS* gfp)) 1))
  
  #|GF256Poly:isZero (gfp)
gfp:GF256Poly
returns a boolean of whether or not the polynomial is equal to 0
|#
  (defun GF256Poly:isZero (gfp)
    (equal (nth 0 (GF256Poly:getCoefficients gfp)) 0))
  
  #|GF256Poly:getCoefficient (gfp degree)
gfp:GF256Poly
degree:number
returns the coefficient of the polynomial for the element with degree "degree"
|#
  (defun GF256Poly:getCoefficient (gfp degree)
    (let* ((coefficients (nth *GF256Poly:COEFFICIENTS* gfp))
           (coeflength (length coefficients)))
      (nth (+ coeflength -1 (* -1 degree)) coefficients)))
  
  #|GF256Poly:sumOfCoefficients (field coefficients a result i)
field:GF256
coefficients:list
a:number
result:number
i:number
returns the sum of the coefficients
|#
  (defun GF256Poly:sumOfCoefficients (field coefficients a result i)
    (if (< i (length coefficients))
        (let* ((newresult (if (equal a 1)
                              result
                              (GF256:multiply field a result)))
               (sum (GF256:addOrSubtract newresult (nth i coefficients))))
          (GF256Poly:sumOfCoefficients field coefficients a sum (+ i 1)))
        result))
  
  #|GF256Poly:evaluateAt (gfp a)
gfp:GF256Poly
a:number
returns the value of the polynomial of gfp at a
|#
  (defun GF256Poly:evaluateAt (gfp a)
    (if (equal a 0)
        (GF256Poly:getCoefficient gfp 0)
        (let* ((field (nth *GF256Poly:FIELD* gfp))
               (coefficients (GF256Poly:getCoefficients gfp)))
          (if (equal a 1)
              (GF256Poly:sumOfCoefficients field coefficients 1 0 0)
              (GF256Poly:sumOfCoefficients field coefficients a (nth 0 coefficients) 1)))))
  
  #|GF256Poly:addOrSubtractLoop1 (smallerCoefficients largerCoefficients lengthDiff i)
helper function for addOrSubtract(gfp other)
|#
  (defun GF256Poly:addOrSubtractLoop1 (smallerCoefficients largerCoefficients lengthDiff i)
    (if (< i (length largerCoefficients))
        (cons (GF256:addOrSubtract (nth (- i lengthDiff) smallerCoefficients) (nth i largerCoefficients))
              (GF256Poly:addOrSubtractLoop1 smallerCoefficients largerCoefficients lengthDiff (+ i 1)))
        nil))
  
  #|GF256Poly:addOrSubtract(gfp other)
gfp:GF256Poly
other:GF256Poly
returns the added polynomials
|#
  (defun GF256Poly:addOrSubtract (gfp other)
    (if (GF256Poly:isZero gfp)
        other
        (if (GF256Poly:isZero other)
            gfp
            (let* ((coefficients (GF256Poly:getCoefficients gfp))
                   (otherCoefficients (GF256Poly:getCoefficients other))
                   (condition (> (length coefficients) (length otherCoefficients)))
                   (smallerCoefficients (if condition
                                            otherCoefficients
                                            coefficients))
                   (largerCoefficients (if condition
                                           coefficients 
                                           otherCoefficients))
                   (lengthDiff (- (length largerCoefficients)
                                  (length smallerCoefficients)))
                   (sumDiff 
                    (append (take lengthDiff largerCoefficients) 
                            (GF256Poly:addOrSubtractLoop1 smallerCoefficients largerCoefficients lengthDiff lengthDiff))))
              (GF256Poly$2 (nth *GF256Poly:FIELD* gfp) sumDiff)))))
  
  #|GF256Poly:multiplyOtherLoop1 (field aCoefficients aLength bCoefficients bLength product i j)
helper function for GF256Poly:multiplyOther
|#
  (defun GF256Poly:multiplyOtherLoop1 (field aCoefficients aLength bCoefficients bLength product i j)
    (if (< i aLength)
        (if (< j bLength)
            (let* ((value (GF256:addOrSubtract (nth (+ i j) product) (GF256:multiply field (nth i aCoefficients) (nth j bCoefficients))))
                   (newproduct (put-nth (+ i j) value product)))
              (GF256Poly:multiplyOtherLoop1 field aCoefficients aLength bCoefficients bLength newproduct i (+ j 1)))
            (GF256Poly:multiplyOtherLoop1 field aCoefficients aLength bCoefficients bLength product (+ i 1) 0))
        product))
  
  #|GF256Poly:multiplyOther (gfp other)
gfp:GF256Poly
other:GF256Poly
returns the two polynomials multipled
|#
  (defun GF256Poly:multiplyOther (gfp other)
    (let* ((field (nth *GF256Poly:FIELD* gfp)))
      (if (or (GF256Poly:isZero gfp) (GF256Poly:isZero other))
          (GF256:getZero field)
          (let* ((aCoefficients (nth *GF256Poly:COEFFICIENTS* gfp))
                 (aLength (length aCoefficients))
                 (bCoefficients (nth *GF256Poly:COEFFICIENTS* other))
                 (bLength (length bCoefficients))
                 (product (createList (+ aLength bLength -1) 0))
                 (finalproduct (GF256Poly:multiplyOtherLoop1 field aCoefficients aLength bCoefficients bLength product 0 0)))
            (GF256Poly$2 field finalproduct)))))
  
  #|GF256Poly:multiplyScalarLoop1 (field coefficients scalar i)
  helper function for GF256Poly:multiplyScalar
|#
  (defun GF256Poly:multiplyScalarLoop1 (field coefficients scalar i)
    (if (< i (length coefficients))
        (cons (GF256:multiply field (nth i coefficients) scalar)
              (GF256Poly:multiplyScalarLoop1 field coefficients scalar (+ i 1)))
        nil))
  
  #|GF256Poly:multiplyScalar (gfp scalar)
gfp:GF256Poly
scalar:number
returns the polynomial multipled by scalar
|#
  (defun GF256Poly:multiplyScalar (gfp scalar)
    (let* ((field (nth *GF256Poly:FIELD* gfp))
           (coefficients (nth *GF256Poly:COEFFICIENTS* gfp)))              
      (if (equal scalar 0)
          (GF256:getZero field)
          (if (equal scalar 1)
              gfp
              (let* ((product (GF256Poly:multiplyScalarLoop1 field coefficients scalar 0)))
                (GF256Poly$2 field product))))))
  
  #|GF256Poly:multiply (gfp value)
gfp:GF256Poly
value:number or GF256Poly
returns multiplies gfp by value
|#
  (defun GF256Poly:multiply (gfp value)
    (if (integerp value)
        (GF256Poly:multiplyScalar gfp value)
        (GF256Poly:multiplyOther gfp value)))
  
  #|GF256Poly:multiplyByMonomial (gfp degree coefficient)
gfp:GF256Poly
degree:number
coefficient:number
returns the polynomial multiplied by a monomial
|#
  (defun GF256Poly:multiplyByMonomial (gfp degree coefficient)
    (let* ((field (nth *GF256Poly:FIELD* gfp))
           (coefficients (nth *GF256Poly:COEFFICIENTS* gfp)))
      (if (equal coefficient 0)
          (GF256:getZero field)
          (let* ((product (append (GF256Poly:multiplyScalarLoop1 field coefficients coefficient 0) (createList degree 0))))
            (GF256Poly$2 field product)))))
  
  #|GF256Poly:divideLoop1 (field other quotient remainder inverseDenominatorLeadingTerm)
helper function for GF256Poly:divide
|#
  (defun GF256Poly:divideLoop1 (field other quotient remainder inverseDenominatorLeadingTerm)
    (let* ((remDegree (GF256Poly:getDegree remainder))
           (otherDegree (GF256Poly:getDegree other)))
      (if (and (>= remDegree otherDegree)
               (not (GF256Poly:isZero remainder)))
          (let* ((degreeDifference (- remDegree otherDegree))
                 (scale (GF256:multiply field (GF256Poly:getCoefficient remainder remDegree) inverseDenominatorLeadingTerm))
                 (term (GF256Poly:multiplyByMonomial other degreeDifference scale))
                 (iterationQuotient (GF256:buildMonomial field degreeDifference scale))
                 (newquotient (GF256Poly:addOrSubtract quotient iterationQuotient))
                 (newremainder (GF256Poly:addOrSubtract remainder term)))
            (GF256Poly:divideLoop1 field other newquotient newremainder inverseDenominatorLeadingTerm))
          (list quotient remainder))))
  
  #|GF256Poly:divide (gfp other)
gfp: GF256Poly
other: GF256Poly
returns the polynomial of gfp/other
|#
  (defun GF256Poly:divide (gfp other)
    (let* ((field (nth *GF256Poly:FIELD* gfp))
           (coefficients (nth *GF256Poly:COEFFICIENTS* gfp))
           (quotient (GF256:getZero field))
           (remainder gfp)
           (denominatorLeadingTerm (GF256Poly:getCoefficient other (GF256Poly:getDegree other)))
           (inverseDenominatorLeadingTerm (GF256:inverse field denominatorLeadingTerm)))
      (GF256Poly:divideLoop1 field other quotient remainder inverseDenominatorLeadingTerm)))
  
  (defun ReedSolomonEncoder$1 (field)
    (list "ReedSolomonEncoder" field))
  
  (defconst *ReedSolomonEncoder:FIELD* 1)
  
  #|ReedSolomonEncoder:buildGeneratorLoop1 (field degree d lastGenerator)
helper function for ReedSolomonEncoder:buildGenerator
|#
  (defun ReedSolomonEncoder:buildGeneratorLoop1 (field degree d lastGenerator)
    (if (<= d degree)
        (let* ((nextGenerator (GF256Poly:multiply lastGenerator (GF256Poly$2 field (list 1 (GF256:exp field (- d 1)))))))
          (ReedSolomonEncoder:buildGeneratorLoop1 field degree (+ d 1) nextGenerator))
        lastGenerator))
  
  #|ReedSolomonEncoder:buildGenerator (rse degree)
rse:the encoder for a specific field
degree: number
returns the polynomial needed for the content being encoded
|#
  (defun ReedSolomonEncoder:buildGenerator (rse degree)
    (let* ((field (nth *ReedSolomonEncoder:FIELD* rse)))
      (ReedSolomonEncoder:buildGeneratorLoop1 field degree 1 (GF256Poly$2 field (list 1)))))
  
  #|ReedSolomonEncoder:encodeLoop1 (toEncode dataBytes numZeroCoefficients i)
  helper function for ReedSolomonEncoder:encode
|#
  (defun ReedSolomonEncoder:encodeLoop1 (toEncode dataBytes numZeroCoefficients i)
    (if (< i numZeroCoefficients)
        (let* ((newtoEncode (put-nth (+ dataBytes i) 0 toEncode)))
          (ReedSolomonEncoder:encodeLoop1 newtoEncode dataBytes numZeroCoefficients (+ i 1)))
        toEncode))
  
  #|ReedSolomonEncoder:encode (rse toEncode ecBytes)
rse:the encoder for a specific field
toEncode:the bytes of the data to encode
ecBytes:the error correction bytes to encode
returns the data and error correction encoded
|#
  (defun ReedSolomonEncoder:encode (rse toEncode ecBytes)
    (let* ((dataBytes (- (length toEncode) ecBytes))
           (generator (ReedSolomonEncoder:buildGenerator rse ecBytes))
           (infoCoefficients (take dataBytes toEncode))
           (info (GF256Poly:multiplyByMonomial (GF256Poly$2 (nth *ReedSolomonEncoder:FIELD* rse) infoCoefficients) ecBytes 1))
           (remainder (nth 1 (GF256Poly:divide info generator)))
           (coefficients (GF256Poly:getCoefficients remainder))
           (numZeroCoefficients (- ecBytes (length coefficients)))
           (newtoEncode (ReedSolomonEncoder:encodeLoop1 toEncode dataBytes numZeroCoefficients 0)))
      (append (take (+ dataBytes numZeroCoefficients) toEncode)
              coefficients)))
  
  (defun BlockPair$2 (data errorCorrection)
    (list "BlockPair" data errorCorrection))
  
  (defconst *BlockPair:DATA_BYTES* 1)
  (defconst *BlockPair:ERROR_CORRECTION_BYTES* 2)
  
  (defun BlockPair:getDataBytes (bp)
    (nth *BlockPair:DATA_BYTES* bp))
  
  (defun BlockPair:getErrorCorrectionBytes (bp)
    (nth *BlockPair:ERROR_CORRECTION_BYTES* bp))
  
  (defconst *Encoder:ALPHANUMERIC_TABLE* 
    (list -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
          -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
          36 -1 -1 -1 37 38 -1 -1 -1 -1 39 40 -1 41 42 43
          0   1  2  3  4  5  6  7  8  9 44 -1 -1 -1 -1 -1
          -1 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
          25 26 27 28 29 30 31 32 33 34 35 -1 -1 -1 -1 -1
          ))
  
  (defun Encoder:calculateMaskPenalty (bytematrix)
    (let* ((rule1 (MaskUtil:applyMaskPenaltyRule1 bytematrix))
           (rule2 (MaskUtil:applyMaskPenaltyRule2 bytematrix))
           (rule3 (MaskUtil:applyMaskPenaltyRule3 bytematrix))
           (rule4 (MaskUtil:applyMaskPenaltyRule4 bytematrix)))
      (+ rule1 rule2 rule3 rule4)))
  
  (defun Encoder:getAlphaNumericCode (code)
    (if (< code (length *Encoder:ALPHANUMERIC_TABLE*))
        (nth code *Encoder:ALPHANUMERIC_TABLE*)
        -1))
  
  (defun Encoder:chooseModeLoop1 (content hasNumeric hasAlphanumeric i)
    (if (< i (length content))
        (let* ((c (nth i content)))
          (if (and (>= (char-code c) (char-code #\0))
                   (<= (char-code c) (char-code #\9)))
              (Encoder:chooseModeLoop1 content 't hasAlphanumeric (+ i 1))
              (if (not (equal (Encoder:getAlphaNumericCode (char-code c)) -1))
                  (Encoder:chooseModeLoop1 content hasNumeric 't (+ i 1))
                  (list nil nil))))
        (list hasNumeric hasAlphanumeric)))
  
  (defun Encoder:chooseMode (content)
    (let* ((mode (Encoder:chooseModeLoop1 (str->chrs content) nil nil 0))
           (hasNumeric (nth 0 mode))
           (hasAlphanumeric (nth 1 mode)))
      (if hasAlphanumeric
          *Mode:ALPHANUMERIC*
          (if hasNumeric
              *Mode:NUMERIC*
              *Mode:BYTE*))))
  
  (defun Encoder:chooseMaskPatternLoop1 
    (bits ecLevel version matrix minPenalty bestMaskPattern maskPattern)
    (if (< maskPattern *QRCode:NUM_MASK_PATTERNS*)
        (let* ((newmatrix (MatrixUtil:buildMatrix 
                           bits ecLevel version maskPattern matrix))
               (penalty (Encoder:calculateMaskPenalty newmatrix)))
          (if (< penalty minPenalty)
              (Encoder:chooseMaskPatternLoop1 
               bits ecLevel version newmatrix 
               penalty maskPattern (+ maskPattern 1))
              (Encoder:chooseMaskPatternLoop1 
               bits ecLevel version newmatrix 
               minPenalty bestMaskPattern (+ maskPattern 1))))
        bestMaskPattern))
  
  (defun Encoder:chooseMaskPattern (bits ecLevel version matrix)
    (let* ((minPenalty 2147483647)
           (bestMaskPattern -1))
      (Encoder:chooseMaskPatternLoop1 
       bits ecLevel version matrix minPenalty bestMaskPattern 0)))
  
  (defun Encoder:initQRCodeLoop1 (numInputBytes ecLevel mode qrCode versionNum)
    (if (<= versionNum 40)
        (let* ((version (Version:getVersionForNumber versionNum))
               (numBytes (Version:getTotalCodewords version))
               (ecBlocks (Version:getECBlocksForLevel version ecLevel))
               (numEcBytes (ECBlocks:getTotalECCodeWords ecBlocks))
               (numRSBlocks (ECBlocks:getNumBlocks ecBlocks))
               (numDataBytes (- numBytes numEcBytes)))
          (if (>= numDataBytes (+ numInputBytes 3))
              (QRCode:setMatrixWidth 
               (QRCode:setNumECBytes 
                (QRCode:setNumRSBlocks 
                 (QRCode:setNumDataBytes 
                  (QRCode:setNumTotalBytes 
                   (QRCode:setVersion qrCode versionNum) 
                   numBytes) 
                  numDataBytes) 
                 numRSBlocks) 
                numEcBytes) 
               (Version:getDimensionForVersion version))
              (Encoder:initQRCodeLoop1 
               numInputBytes ecLevel mode qrCode (+ versionNum 1))))
        nil))
  
  (defun Encoder:initQRCode (numInputBytes ecLevel mode qrCode)
    (Encoder:initQRCodeLoop1 
     numInputBytes ecLevel mode 
     (QRCode:setMode 
      (QRCode:setECLevel qrCode ecLevel) 
      mode) 
     1))
  
  (defun Encoder:terminateBitsLoop3 (bits numPaddingBytes i)
    (if (< i numPaddingBytes)
        (let* ((condition (equal (logand i 1) 0))
               (value (if condition 236 17))
               (newbits (BitArray:appendBits bits value 8)))
          (Encoder:terminateBitsLoop3 newbits numPaddingBytes (+ i 1)))
        bits))
  
  (defun Encoder:terminateBitsLoop2 (bits i)
    (if (< i 8)
        (let* ((newbits (BitArray:appendBit bits nil)))
          (Encoder:terminateBitsLoop2 newbits (+ i 1)))
        bits))
  
  (defun Encoder:terminateBitsLoop1 (bits capacity i)
    (if (and (< i 4) (< (BitArray:getSize bits) capacity))
        (let* ((newbits (BitArray:appendBit bits nil)))
          (Encoder:terminateBitsLoop1 newbits capacity (+ i 1)))
        bits))
  
  (defun Encoder:terminateBits (numDataBytes bits)
    (let* ((capacity (<< numDataBytes 3))
           (newbits (Encoder:terminateBitsLoop1 bits capacity 0))
           (numBitsInLastByte (logand (BitArray:getSize newbits) 7))
           (terminateBits (if (> numBitsInLastByte 0)
                              (Encoder:terminateBitsLoop2 
                               newbits numBitsInLastByte)
                              newbits))
           (numPaddingBytes (- numDataBytes 
                               (BitArray:getSizeInBytes terminateBits)))
           (i 0))
      (Encoder:terminateBitsLoop3 terminateBits numPaddingBytes i)))
  
  (defun Encoder:getNumDataBytesAndNumECBytesForBlockID 
    (numTotalBytes numDataBytes numRSBlocks blockID)
    (let* ((numRsBlocksInGroup2 (mod numTotalBytes numRSBlocks))
           (numRsBlocksInGroup1 (- numRSBlocks numRsBlocksInGroup2))
           (numTotalBytesInGroup1 (truncate numTotalBytes numRSBlocks))
           (numTotalBytesInGroup2 (+ numTotalBytesInGroup1 1))
           (numDataBytesInGroup1 (truncate numDataBytes numRSBlocks))
           (numDataBytesInGroup2 (+ numDataBytesInGroup1 1))
           (numEcBytesInGroup1 (- numTotalBytesInGroup1 numDataBytesInGroup1))
           (numEcBytesInGroup2 (- numTotalBytesInGroup2 numDataBytesInGroup2))
           (newnumDataBytesInBlock (if (< blockID numRsBlocksInGroup1)
                                       numDataBytesInGroup1
                                       numDataBytesInGroup2))
           (newnumECBytesInBlock (if (< blockID numRsBlocksInGroup1)
                                     numEcBytesInGroup1
                                     numEcBytesInGroup2)))
      (list newnumDataBytesInBlock newnumECBytesInBlock)))
  
  (defun Encoder:generateECBytesLoop2 
    (numEcBytesInBlock numDataBytes toEncode i)
    (if (< i numEcBytesInBlock)
        (let* ((value (nth (+ numDataBytes i) toEncode))
               (byte (if (> value 128) (- value 256) value)))
          (cons byte 
                (Encoder:generateECBytesLoop2 
                 numEcBytesInBlock numDataBytes toEncode (+ i 1))))
        nil))
  
  (defun Encoder:generateECBytesLoop1 (dataBytes numDataBytes i)
    (if (< i numDataBytes)
        (cons (logand (nth i dataBytes) 255) 
              (Encoder:generateECBytesLoop1 dataBytes numDataBytes (+ i 1)))
        nil))
  
  (defun Encoder:generateECBytes (dataBytes numEcBytesInBlock)
    (let* ((numDataBytes (length dataBytes))
           (toEncode (append 
                      (Encoder:generateECBytesLoop1 dataBytes numDataBytes 0) 
                      (createList numEcBytesInBlock 0)))
           (reedEncoder (ReedSolomonEncoder$1 *GF256:QR_CODE_FIELD*))
           (encoded (ReedSolomonEncoder:encode 
                     reedEncoder toEncode numEcBytesInBlock)))
      (Encoder:generateECBytesLoop2 numEcBytesInBlock numDataBytes encoded 0)))
  
  (defun Encoder:generateECBlocks 
    (bits numTotalBytes numDataBytes numRSBlocks values i)
    (if (< i numRSBlocks)
        (let* ((dataBytesOffset (nth 0 values))
               (maxNumDataBytes (nth 1 values))
               (maxNumEcBytes (nth 2 values))
               (blocks (nth 3 values))
               (BytesInBlock (Encoder:getNumDataBytesAndNumECBytesForBlockID
                              numTotalBytes numDataBytes numRSBlocks i))
               (numDataBytesInBlock (nth 0 BytesInBlock))
               (numEcBytesInBlock (nth 1 BytesInBlock))
               (size numDataBytesInBlock)
               (dataBytes (BitArray:toBytes bits (* 8 dataBytesOffset) 
                                            (createList size 0) 0 size))
               (ecBytes (Encoder:generateECBytes dataBytes numEcBytesInBlock))
               (newblocks (cons (BlockPair$2 dataBytes ecBytes) blocks))
               (newmaxNumDataBytes (max maxNumDataBytes size))
               (newmaxNumEcBytes (max maxNumEcBytes (length ecBytes)))
               (newdataBytesOffset (+ dataBytesOffset numDataBytesInBlock))
               (newvalues 
                (list newdataBytesOffset newmaxNumDataBytes 
                      newmaxNumEcBytes newblocks)))
          (Encoder:generateECBlocks 
           bits numTotalBytes numDataBytes numRSBlocks newvalues (+ i 1)))
        (put-nth 3 (reverse (nth 3 values)) values)))
  
  (defun Encoder:generateDataBlocksLoop1 (result blocks i j)
    (if (< j (length blocks))
        (let* ((dataBytes (BlockPair:getDataBytes (nth j blocks)))
               (newresults (if (< i (length dataBytes))
                               (BitArray:appendBits result (nth i dataBytes) 8)
                               result)))
          (Encoder:generateDataBlocksLoop1 newresults blocks i (+ j 1)))
        result))
  
  (defun Encoder:generateDataBlocks (result maxNumDataBytes blocks i)
    (if (< i maxNumDataBytes)
        (let* ((j 0)
               (newresults 
                (Encoder:generateDataBlocksLoop1 result blocks i j)))
          (Encoder:generateDataBlocks 
           newresults maxNumDataBytes blocks (+ i 1)))
        result))
  
  (defun Encoder:generateErrorBlocksLoop1 (result blocks i j)
    (if (< j (length blocks))
        (let* ((ecBytes (BlockPair:getErrorCorrectionBytes (nth j blocks)))
               (newresults (if (< i (length ecBytes))
                               (BitArray:appendBits result (nth i ecBytes) 8)
                               result)))
          (Encoder:generateErrorBlocksLoop1 newresults blocks i (+ j 1)))
        result))
  
  (defun Encoder:generateErrorBlocks (result maxNumEcBytes blocks i)
    (if (< i maxNumEcBytes)
        (let* ((j 0)
               (newresults 
                (Encoder:generateErrorBlocksLoop1 result blocks i j)))
          (Encoder:generateErrorBlocks 
           newresults maxNumEcBytes blocks (+ i 1)))
        result))  
  
  (defun Encoder:interleaveWithECBytes 
    (bits numTotalBytes numDataBytes numRSBlocks result)
    (let* ((values (list 0 0 0 nil))
           (newvalues (Encoder:generateECBlocks 
                       bits numTotalBytes numDataBytes numRSBlocks values 0))
           (dataBytesOffset (nth 0 newvalues))
           (maxNumDataBytes (nth 1 newvalues))
           (maxNumEcBytes (nth 2 newvalues))
           (blocks (nth 3 newvalues))
           (dataBlocks 
            (Encoder:generateDataBlocks result maxNumDataBytes blocks 0))
           (errorBlocks 
            (Encoder:generateErrorBlocks dataBlocks maxNumEcBytes blocks 0)))
      errorBlocks))
  
  (defun Encoder:appendModeInfo (mode bits)
    (BitArray:appendBits bits (Mode:getBits mode) 4))
  
  (defun Encoder:appendLengthInfo (numLetters version mode bits)
    (let* ((numBits (Mode:getCharacterCountBits 
                     mode (Version:getVersionForNumber version))))
      (BitArray:appendBits bits numLetters numBits)))
  
  (defun Encoder:append8BitBytesLoop (bits bytes i)
    (if (< i (length bytes))
        (let* ((8bits (BitArray:appendBits bits (nth i bytes) 8)))
          (Encoder:append8BitBytesLoop 8bits bytes (+ i 1)))
        bits))
  
  (defun Encoder:append8BitBytes (content bits)
    (let* ((bytes (getBytes content)))
      (Encoder:append8BitBytesLoop bits bytes 0)))
  
  (defun Encoder:appendAlphanumericBytesLoop (content bits i)
    (if (< i (length content))
        (let* ((code1 (Encoder:getAlphanumericCode 
                       (nth i (getBytes content)))))
          (if (< (+ i 1) (length content))
              (let* ((code2 (Encoder:getAlphanumericCode 
                             (nth (+ i 1) (getBytes content))))
                     (11bits (BitArray:appendBits 
                              bits (+ (* code1 45) code2) 11)))
                (Encoder:appendAlphanumericBytesLoop content 11bits (+ i 2)))
              (let* ((6bits (BitArray:appendBits bits code1 6)))
                (Encoder:appendAlphanumericBytesLoop content 6bits (+ i 1)))))
        bits))
  
  (defun Encoder:appendAlphanumericBytes (content bits)
    (Encoder:appendAlphanumericBytesLoop content bits 0))
  
  (defun Encoder:appendNumericBytesLoop (content bits i)
    (if (< i (length content))
        (let* ((num1 (chr->dgt (nth i (str->chrs content)))))
          (if (< (+ i 2) (length content))
              (let* ((num2 (chr->dgt (nth (+ i 1) (str->chrs content))))
                     (num3 (chr->dgt (nth (+ i 2) (str->chrs content))))
                     (10bits (BitArray:appendBits 
                              bits (+ (* num1 100) (* num2 10) num3) 10)))
                (Encoder:appendNumericBytesLoop content 10bits (+ i 3)))
              (if (< (+ i 1) (length content))
                  (let* ((num2 (chr->dgt (nth (+ i 1) (str->chrs content))))
                         (7bits (BitArray:appendBits 
                                 bits (+ (* num1 10) num2) 7)))
                    (Encoder:appendNumericBytesLoop content 7bits (+ i 2)))
                  (let* ((4bits (BitArray:appendBits bits num1 4)))
                    (Encoder:appendNumericBytesLoop content 4bits (+ i 1)))))) 
        bits))
  
  (defun Encoder:appendNumericBytes (content bits)
    (Encoder:appendNumericBytesLoop content bits 0))
  
  (defun Encoder:appendBytes (content mode bits)
    (if (equal mode *Mode:NUMERIC*)
        (Encoder:appendNumericBytes content bits)
        (if (equal mode *Mode:ALPHANUMERIC*)
            (Encoder:appendAlphaNumericBytes content bits)
            (if (equal mode *Mode:BYTE*)
                (Encoder:append8BitBytes content bits)
                nil))))
  
  #|Encoder:encode (content ecLevel)
content:string
ecLevel:ErrorCorrectionLevel
returns a matrix with the content encoded with an error correction of ecLevel
|#
  (defun Encoder:encode (content ecLevel)
    (let* ((qrCode (QRCode))
           (mode (Encoder:chooseMode content))
           (dataBits (Encoder:appendBytes content mode (BitArray)))
           (numInputBytes (BitArray:getSizeInBytes dataBits))
           (code (Encoder:initQRCode numInputBytes ecLevel mode qrCode))
           (modeBits (Encoder:appendModeInfo mode (BitArray)))
           (numLetters (if (equal mode *Mode:BYTE*)
                           (BitArray:getSizeInBytes dataBits)
                           (length content)))
           (lengthBits (Encoder:appendLengthInfo 
                        numLetters (QRCode:getVersion code) mode modeBits))
           (header&bodyBits (BitArray:appendBitArray lengthBits dataBits))
           (terminateBits (Encoder:terminateBits 
                           (QRCode:getNumDataBytes code) header&bodyBits))
           (finalBits (Encoder:interleaveWithECBytes 
                       terminateBits (QRCode:getNumTotalBytes code) 
                       (QRCode:getNumDataBytes code) 
                       (QRCode:getNumRSBlocks code) (BitArray)))
           (matrix (ByteMatrix$2 (QRCode:getMatrixWidth code) 
                                 (QRCode:getMatrixWidth code)))
           (codeWithPattern 
            (QRCode:setMaskPattern 
             code (Encoder:chooseMaskPattern 
                   finalBits (QRCode:getECLevel code) 
                   (QrCode:getVersion code) matrix)))
           (builtMatrix (MatrixUtil:buildMatrix 
                         finalBits (QRCode:getECLevel codeWithPattern) 
                         (QRCode:getVersion codeWithPattern) 
                         (QRCode:getMaskPattern codeWithPattern) matrix)))
      (QRCode:setMatrix codeWithPattern builtMatrix)))
  
  (export IEncoder))