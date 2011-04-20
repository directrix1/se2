;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
(require "IQRCode.lisp")
(require "IMatrix.lisp")
(require "IUtilities.lisp")
(module MQRCode
  (import IMatrix)
  (import IUtilities)
  
  #| QRCode
return: an empty QR Code data structure
|#
  (defun QRCode ()
    (let* ((mode nil)
           (ecLevel nil)
           (version -1)
           (matrixWidth -1)
           (maskPattern -1)
           (numTotalBytes -1)
           (numDataBytes -1)
           (numECBytes -1)
           (numRSBlocks -1)
           (matrix nil))
      (list "QRCode" mode ecLevel version matrixWidth maskPattern numTotalBytes numDataBytes numECBytes numRSBlocks matrix)))
  
  (defconst *QRCODE:NUM_MASK_PATTERNS* 8)
  
  (defconst *QRCode:MODE* 1)
  (defconst *QRCode:EC_LEVEL* 2)
  (defconst *QRCode:VERSION* 3)
  (defconst *QRCode:MATRIX_WIDTH* 4)
  (defconst *QRCode:MASK_PATTERN* 5)
  (defconst *QRCode:NUM_TOTAL_BYTES* 6)
  (defconst *QRCode:NUM_DATA_BYTES* 7)
  (defconst *QRCode:NUM_EC_BYTES* 8)
  (defconst *QRCode:NUM_RS_BLOCKS* 9)
  (defconst *QRCode:MATRIX* 10)
  
  #| QRCode:getMode
code: the QR Code we want the mode for
return: the mode for the passed in QR Code
|#
  (defun QRCode:getMode (code)
    (nth *QRCode:MODE* code))
  
  #| QRCode:getECLevel
code: the QR Code we want the Error correction level for
return: the error correction level for the QR Code passed in
|#
  (defun QRCode:getECLevel (code)
    (nth *QRCode:EC_LEVEL* code))
  
  #| QRCode:getVersion
code: the QR Code we want the version for
return: the version of the QR Code that was passed in
|#
  (defun QRCode:getVersion (code)
    (nth *QRCode:VERSION* code))
  
  #| QRCode:getMatrixWidth
code: the QR Code that we want the matrix width for
return: the matrix width for the QR Code passed in
|#
  (defun QRCode:getMatrixWidth (code)
    (nth *QRCode:MATRIX_WIDTH* code))
  
  #| QRCode:getMaskPattern
code: the QR Code that we want the Mask Pattern for
return: the mask pattern for the QR Code passed in
|#
  (defun QRCode:getMaskPattern (code)
    (nth *QRCode:MASK_PATTERN* code))
  
  #| QRCode:getNumTotalBytes
code: the QR Code we want the total number of bytes for
return: the total number of bytes for the QR Code passed in
|#
  (defun QRCode:getNumTotalBytes (code)
    (nth *QRCode:NUM_TOTAL_BYTES* code))
  
  #| QRCode:getNumDataBytes
code: the QR Code we want the number of data bytes for
return: the number of data bytes for the QR Code passed in
|#
  (defun QRCode:getNumDataBytes (code)
    (nth *QRCode:NUM_DATA_BYTES* code))
  
  #| QRCode:getNumECBytes
code: the QR Code we want the number of error correction bytes
return: the number of error correction bytes for the QR Code passed in
|#
  (defun QRCode:getNumECBytes (code)
    (nth *QRCode:NUM_EC_BYTES* code))
  
  #| QRCode:getNumRSBlocks
code: the QR Code we want the number of Reed Soloman bytes for
return: the number of Reed Soloman bytes for the QR Code passed in
|#
  (defun QRCode:getNumRSBlocks (code)
    (nth *QRCode:NUM_RS_BLOCKS* code))
  
  #| QRCode:getMatrix
code: the QR Code we want the matrix for
return: the matrix of bits for the QR Code passed in
|#
  (defun QRCode:getMatrix (code)
    (nth *QRCode:MATRIX* code))
  
  #| QRCode:at
code: the QR Code we want the pixel information from
x: the x coordinant of the pixel we want
y: the y coordinant of the pixel we want
return: the value of the bit in the QR Code matrix at the x,y given
|#
  (defun QRCode:at (code x y)
    (ByteMatrix:get (QRCode:getMatrix code) x y))
  
  #| QRCode:isValidMaskPattern
maskPattern: the mask pattern number we want to know is valid or not
return: boolean (nil or not-nil) representing whether or not the given mask pattern is valid
|#
  (defun QRCode:isValidMaskPattern (maskPattern)
    (and (>= maskPattern 0) (< maskPattern *QRCode:NUM_MASK_PATTERNS*)))
  
  #| QRCode:isValid
code: the QR Code we want to know is valid or not
return: boolean representing whether or not the given mask pattern is valid or not
goes through each part of the QR Code structure and makes sure each part is valid
|#
  (defun QRCode:isValid (code)
    (let* ((validMode (not (null (QRCode:getMode code))))
           (validECLevel (not (null (QRCode:getECLevel code))))
           (validVersion (not (equal (QRCode:getVersion code) -1)))
           (validWidth (not (equal (QRCode:getMatrixWidth code) -1)))
           (validMaskPattern (not (equal (QRCode:getMaskPattern code) -1)))
           (validNumTotalBytes (not (equal (QRCode:getNumTotalBytes code) -1)))
           (validNumDataBytes (not (equal (QRCode:getNumDataBytes code) -1)))
           (validNumECBytes (not (equal (QRCode:getNumECBytes code) -1)))
           (validNumRSBlocks (not (equal (QRCode:getNumRSBlocks code) -1)))
           (validMatrix (not (null (QRCode:getMatrix code)))))
      (and validMode validECLevel validVersion validWidth validMaskPattern validNumTotalBytes validNumDataBytes validNumECBytes validNumRSBlocks validMatrix (QRCode:isValidMaskPattern (QRCode:getMaskPattern code)) (equal (QRCode:getNumTotalBytes code) (+ (QRCode:getNumDataBytes code) (QRCode:getNumECBytes code))) (equal (QRCode:getMatrixWidth code) (ByteMatrix:getWidth (QRCode:getMatrix code))) (equal (ByteMatrix:getWidth (QRCode:getMatrix code)) (ByteMatrix:getHeight (QRCode:getMatrix code))))))
  
  #| QRCode:setMode
code: the QR Code we want to set the mode of
value: the mode value that should be set in the QR Code passed in
return: a copy of the QR Code passed in but with it's mode set to the value passed in
|#
  (defun QRCode:setMode (code value)
    (put-nth *QRCode:MODE* value code))
  
  #| QRCode:setECLevel
code: the QR Code we want to set the error Correction for
value: the value the passed in QR Code's error correction should be
return: a copy of the QR Code passed in but with it's error correction set to the value passed in
|#
  (defun QRCode:setECLevel (code value)
    (put-nth *QRCode:EC_LEVEL* value code))
  
  #| QRCode:setVersion
code: the QR Code we want to set the version for
value: the value that the passed in QR Code's version should be
return: a copy of the QR Code passed in but with it's version set to the value passed in
|#
  (defun QRCode:setVersion (code value)
    (put-nth *QRCode:VERSION* value code))
  
  #| QRCode:setMatrixWidth
code: the QR Code we want to set the matrix width for
value: the value the passed in QR Code's matrix width should be
return: a copy of the QR Code passed in but with it's matrix width set to the value passed in
|#
  (defun QRCode:setMatrixWidth (code value)
    (put-nth *QRCode:MATRIX_WIDTH* value code))
  
  #| QRCode:setMaskPattern
code: the QR Code we want to set the mask pattern for
value: the value the passed in QR Code's Mask Pattern should be
return: a copy of the QR Code passed in but with it's mask pattern set to the value passed in
|#
  (defun QRCode:setMaskPattern (code value)
    (put-nth *QRCode:MASK_PATTERN* value code))
  
  #| QRCode:setNumTotalBytes
code: the QR Code we want to set the number of total bytes for
value: the value the passed in QR Code's number of total bytes should be
return: a copy of the QR Code passed in but with it's number of total bytes set to the value passed in
|#
  (defun QRCode:setNumTotalBytes (code value)
    (put-nth *QRCode:NUM_TOTAL_BYTES* value code))
  
  #| QRCode:setNumDataBytes
code: the QR Code we want to set the number of data bytes for
value: the value the passed in QR Code's number of data bytes should be
return: a copy of the QR Code passed in but with it's number of data bytes set to the value passed in
|#
  (defun QRCode:setNumDataBytes (code value)
    (put-nth *QRCode:NUM_DATA_BYTES* value code))
  
  #| QRCode:setNumECBytes
code: the QR Code we want to change the number of error correction bytes for
value: the value the passed in QR Code's number of error correction bytes should be
return: a copy of the QR Code passed in but with it's number of error correction bytes set to the value passed in
|#
  (defun QRCode:setNumECBytes (code value)
    (put-nth *QRCode:NUM_EC_BYTES* value code))
  
  #| QRCode:setNumRSBlocks
code: the QR Code we want to set the number of reed soloman bytes for
value: the value the passed in QR Code's number of RS bytes should be
return: a copy of the QR Code passed in but with it's number of RS bytes set to the value passed in
|#
  (defun QRCode:setNumRSBlocks (code value)
    (put-nth *QRCode:NUM_RS_BLOCKS* value code))
  
  #| QRCode:setMatrix
code: the QR Code we want to set the matrix of
value: the matrix the passed in QR Code should have
return: a copy of the QR Code passed in but with it's matrix set to the value passed in
|#
  (defun QRCode:setMatrix (code value)
    (put-nth *QRCode:MATRIX* value code))
  
  #| ErrorCorrectionLevel$3
ordinal: the order in which the error correction level is sorted by LMQH
bits: the bit representation of the error correction level
name: the name the error correction level goes by (LMQ or H)
return: the data structure we use for Error Correction information
|#
  (defun ErrorCorrectionLevel$3 (ordinal bits name)
    (list "ErrorCorrectionLevel" ordinal bits name))
  
  ;L = ~7% correction
  (defconst *ErrorCorrectionLevel:L* (ErrorCorrectionLevel$3 0 1 "L"))
  ;M = ~15% correction
  (defconst *ErrorCorrectionLevel:M* (ErrorCorrectionLevel$3 1 0 "M"))
  ;Q = ~25% correction
  (defconst *ErrorCorrectionLevel:Q* (ErrorCorrectionLevel$3 2 3 "Q"))
  ;H = ~30% correction
  (defconst *ErrorCorrectionLevel:H* (ErrorCorrectionLevel$3 3 2 "H"))
  
  (defconst *ErrorCorrectionLevel:FORBITS* 
    (list *ErrorCorrectionLevel:M* 
          *ErrorCorrectionLevel:L* 
          *ErrorCorrectionLevel:H* 
          *ErrorCorrectionLevel:Q*))
  
  (defconst *ErrorCorrectionLevel:ORDINAL* 1)
  (defconst *ErrorCorrectionLevel:BITS* 2)
  (defconst *ErrorCorrectionLevel:NAME* 3)
  
  #| ErrorCorrectionLevel:ordinal
ecl: the error correction we want to find the ordinal of
return: the ordinal of the passed in Error correction type
|#
  (defun ErrorCorrectionLevel:ordinal (ecl)
    (nth *ErrorCorrectionLevel:ORDINAL* ecl))
  
  #| ErrorCorrectioLevel:getBits
ecl: the error correction we want to find the bits of
return: the bits of the passed in error correction type
|#
  (defun ErrorCorrectionLevel:getBits (ecl)
    (nth *ErrorCorrectionLevel:BITS* ecl))
  
  #| ErrorCorrectionLevel:getName
ecl: the error correction we want to get the name of
return: the name of the passed in error correction type
|#
  (defun ErrorCorrectionLevel:getName (ecl)
    (nth *ErrorCorrectionLevel:NAME* ecl))
  
  #| ErrorCorrectionLevel:forBits
bits: the index of FORBIT we want to get an error correction out of 
return: the error correction level based on the bits passed in from the FORBITS struct
|#
  (defun ErrorCorrectionLevel:forBits (bits)
    (nth bits *ErrorCorrectionLevel:FORBITS*))
  
  #| FormatInformation$1
formatInfo: which Error correction bits to use to generate a QR Code's format information
return: a formatInformation type (errorcorrection + datamask)
|#
  (defun FormatInformation$1 (formatInfo)
    (let* ((errorCorrectionLevel 
            (ErrorCorrectionLevel:forBits (logand (>> formatInfo 3) 3)))
           (dataMask (logand formatInfo 7)))
      (list "FormatInformation" errorCorrectionLevel dataMask)))
  
  (defconst *FormatInformation:FORMAT_INFO_MASK_QR* 21522)
  
  (defconst *FormatInformation:FORMAT_INFO_DECODE_LOOKUP*
    (list (list 21522 0)
          (list 20773 1)
          (list 24188 2)
          (list 23371 3)
          (list 17913 4)
          (list 16590 5)
          (list 20375 6)
          (list 19104 7)
          (list 30660 8)
          (list 29427 9)
          (list 32170 10)
          (list 30877 11)
          (list 26159 12)
          (list 25368 13)
          (list 27713 14)
          (list 26998 15)
          (list 5769 16)
          (list 5054 17)
          (list 7399 18)
          (list 6608 19)
          (list 1890 20)
          (list 597 21)
          (list 3340 22)
          (list 2107 23)
          (list 13663 24)
          (list 12392 25)
          (list 16177 26)
          (list 14854 27)
          (list 9396 28)
          (list 8579 29)
          (list 11994 30)
          (list 11245 31)))
  
  (defconst *FormatInformation:BITS_SET_IN_HALF_BYTE*
    (list 0 1 1 2 1 2 2 3 1 2 2 3 2 3 3 4))
  
  (defconst *FormatInformation:ERROR_CORRECTION_LEVEL* 1)
  (defconst *FormatInformation:DATA_MASK* 2)
  
  #| FormatInformation:numBitsDiffering
a: a matrix of bits
b: another matrix of bits
return: The number of bits different between the two paramaters
|#
  (defun FormatInformation:numBitsDiffering (a b)
    (let* ((axorb (logxor a b)))
      (+ (nth (logand axorb 15) 
              *FormatInformation:BITS_SET_IN_HALF_BYTE*)
         (nth (logand (>>> axorb 4) 15) 
              *FormatInformation:BITS_SET_IN_HALF_BYTE*)
         (nth (logand (>>> axorb 8) 15) 
              *FormatInformation:BITS_SET_IN_HALF_BYTE*)
         (nth (logand (>>> axorb 12) 15) 
              *FormatInformation:BITS_SET_IN_HALF_BYTE*)
         (nth (logand (>>> axorb 16) 15) 
              *FormatInformation:BITS_SET_IN_HALF_BYTE*)
         (nth (logand (>>> axorb 20) 15) 
              *FormatInformation:BITS_SET_IN_HALF_BYTE*)
         (nth (logand (>>> axorb 24) 15) 
              *FormatInformation:BITS_SET_IN_HALF_BYTE*)
         (nth (logand (>>> axorb 28) 15) 
              *FormatInformation:BITS_SET_IN_HALF_BYTE*))))
  
  #| FormatInformation:doDecodeFormatInformationLoop1
maskedFormatInfo1: format info indicator, with mask still applied
maskedFormatInfo2: second copy of the same info
bestDifference: the best (least) difference between the masks checked so far
bestFormatInfo: the format info for the mask with the best difference
i: loop iterator
return:a list of the parameters above, updated
|#
  (defun FormatInformation:doDecodeFormatInformationLoop1 
    (maskedFormatInfo1 maskedFormatInfo2 bestDifference bestFormatInfo i)
    (if (< i (length *FormatInformation:FORMAT_INFO_DECODE_LOOKUP*))
        (let* ((decodeInfo 
                (nth i *FormatInformation:FORMAT_INFO_DECODE_LOOKUP*))
               (targetInfo (nth 0 decodeInfo)))
          (if (or (equal targetInfo maskedFormatInfo1) 
                  (equal targetInfo maskedFormatInfo2))
              (FormatInformation$1 (nth 1 decodeInfo))
              (let* ((bitsDifference1 
                      (FormatInformation:numBitsDiffering 
                       maskedFormatInfo1 targetInfo))
                     (bestFormatInfo1 (if (< bitsDifference1 bestDifference)
                                          (nth 1 decodeInfo)
                                          bestFormatInfo))
                     (bestDifference1 (if (< bitsDifference1 bestDifference)
                                          bitsDifference1
                                          bestDifference)))
                (if (not (equal maskedFormatInfo1 maskedFormatInfo2))
                    (let* ((bitsDifference2 
                            (FormatInformation:numBitsDiffering 
                             maskedFormatInfo2 targetInfo))
                           (bestFormatInfo2 
                            (if (< bitsDifference2 bestDifference) 
                                (nth 1 decodeInfo) bestFormatInfo1))
                           (bestDifference2 
                            (if (< bitsDifference2 bestDifference) 
                                bitsDifference2 bestDifference1)))
                      (FormatInformation:doDecodeFormatInformationLoop1 
                       maskedFormatInfo1 maskedFormatInfo2 bestDifference2
                       bestFormatInfo2 (+ i 1)))
                    (FormatInformation:doDecodeFormatInformationLoop1
                     maskedFormatInfo1 maskedFormatInfo2 bestDifference1 
                     bestFormatInfo1 (+ i 1))))))
        (if (<= bestDifference 3)
            (FormatInformation$1 bestFormatInfo)
            nil)))
  
  #| FormatInformation:doDecodeFormatInformation
maskedFormatInfo1: format info indicator, with mask still applied
maskedFormatInfo2: second copy of the same info
return: the best mask format info for QR Code
|#
  (defun FormatInformation:doDecodeFormatInformation 
    (maskedFormatInfo1 maskedFormatInfo2)
    (let* ((bestDifference 2147483647)
           (bestFormatInfo 0))
      (FormatInformation:doDecodeFormatInformationLoop1 
       maskedFormatInfo1 maskedFormatInfo2 bestDifference bestFormatInfo 0)))
  
  #| FormatInformation:decodeFormatInformation
maskedFormatInfo1: format info indicator, with mask still applied
maskedFormatInfo2: second copy of the same info
return: decoded format information
|#
  (defun FormatInformation:decodeFormatInformation 
    (maskedFormatInfo1 maskedFormatInfo2)
    (let* ((formatInfo 
            (FormatInformation:doDecodeFormatInformation 
             maskedFormatInfo1 maskedFormatInfo2)))
      (if (not (null formatInfo))
          formatInfo
          (FormatInformation:doDecodeFormatInformation 
           (logxor maskedFormatInfo1 *FormatInformation:FORMAT_INFO_MASK_QR*) 
           (logxor maskedFormatInfo2 *FormatInformation:FORMAT_INFO_MASK_QR*)))))
  
  #|FormatInformation:getErrorCorrectionLevel
fi: the formatInformation to get the errorCorrection level for
return: the errorCorrection level of the format information passed in
|#
  (defun FormatInformation:getErrorCorrectionLevel (fi)
    (nth *FormatInformation:ERROR_CORRECTION_LEVEL* fi))
  
  #| FormatInformation:getDataMask
fi: the format information to get the data mask for
return: the data mask used on the passed in format information
|#
  (defun FormatInformation:getDataMask (fi)
    (nth *FormatInformation:DATA_MASK* fi))
  
  (defconst *ECB:COUNT* 1)
  (defconst *ECB:DATA_CODE_WORDS* 2)
  
  #| ECB:getCount
ecb: the error correction blocks to get the count for
return: the count for the error blocks passed in
|#
  (defun ECB:getCount (ecb)
    (nth *ECB:COUNT* ecb))
  
  #| ECB:getDataCodewords
ecb: the errorcorrection blocks to get the data code words for
return: the data code words for the error correction blocks passed in
|#
  (defun ECB:getDataCodewords (ecb)
    (nth *ECB:DATA_CODE_WORDS* ecb))
  
  (defconst *ECBlocks:EC_CODE_WORDS_PER_BLOCK* 1)
  (defconst *ECBlocks:EC_BLOCKS* 2)
  
  #| ECBlocks:getECCodewordsPerBlock	
ecBlocks: the Blocks to find the error correction code words for
return: the ECCodewords for the Blocks passed in
|#
  (defun ECBlocks:getECCodewordsPerBlock (ecBlocks) 
    (nth *ECBlocks:EC_CODE_WORDS_PER_BLOCK* ecBlocks))
  
  #| ECBlocks:getNumBlocksLoop
ecBlocks: the error correction blocks to count
i: iterator/shift register for the loop
return: number of blocks on ecBlocks
|#
  (defun ECBlocks:getNumBlocksLoop (ecBlocks i)
    (if (< i (length (nth *ECBlocks:EC_BLOCKS* ecBlocks)))
        (let* ((total (ECB:getCount 
                       (nth i (nth *ECBlocks:EC_BLOCKS* ecBlocks)))))
          (+ total (ECBlocks:getNumBlocksLoop ecBlocks (+ i 1))))
        0))
  
  #|ECBlocks:getNumBlocks
ecBlocks: the error correction blocks to count blocks for
return: the number of blocks in the passed in error correction blocks
|#
  (defun ECBlocks:getNumBlocks (ecBlocks)
    (ECBlocks:getNumBlocksLoop ecBlocks 0))
  
  #| ECBlocks:getTotalECCodewords
ecBlocks: blocks to get the total error correction codewords for
return: the total error correction code words in the ECblocks passed in
|#
  (defun ECBlocks:getTotalECCodewords (ecBlocks)
    (let* ((ecCodewordsPerBlock 
            (nth *ECBlocks:EC_CODE_WORDS_PER_BLOCK* ecBlocks))
           (numBlocks (ECBlocks:getNumBlocks ecBlocks)))
      (* ecCodewordsPerBlock numBlocks)))
  
  #| ECBlocks:getECBlocks
ecBlocks: the error correction blocks type to get the error correction blocks from
return:the Error correction blocks from an ecBlocks type
|#
  (defun ECBlocks:getECBlocks (ecBlocks)
    (nth *ECBlocks:EC_BLOCKS* ecBlocks))
  
  (defconst *Version:VERSION_DECODE_INFO*
    (list 31892 34236 39577 42195 48118
          51042 55367 58893 63784 68472
          70749 76311 79154 84390 87683
          92361 96236 102084 102881 110507
          110734 117786 119615 126325 127568
          133589 136944 141498 145311 150283
          152622 158308 161089 167017))
  
  (defconst *Version:VERSION_NUMBER* 1)
  (defconst *Version:ALIGNMENT_PATTERN_CENTERS* 2)
  (defconst *Version:EC_BLOCKS* 3)
  (defconst *Version:TOTAL_CODE_WORDS* 4)
  
  (defconst *Version:VERSIONS* 
    '(("Version"
       1
       nil
       (("ECBlocks" 7 (("ECB" 1 19)))
        ("ECBlocks" 10 (("ECB" 1 16)))
        ("ECBlocks" 13 (("ECB" 1 13)))
        ("ECBlocks" 17 (("ECB" 1 9))))
       26)
      ("Version"
       2
       (6 18)
       (("ECBlocks" 10 (("ECB" 1 34)))
        ("ECBlocks" 16 (("ECB" 1 28)))
        ("ECBlocks" 22 (("ECB" 1 22)))
        ("ECBlocks" 28 (("ECB" 1 16))))
       44)
      ("Version"
       3
       (6 22)
       (("ECBlocks" 15 (("ECB" 1 55)))
        ("ECBlocks" 26 (("ECB" 1 44)))
        ("ECBlocks" 18 (("ECB" 2 17)))
        ("ECBlocks" 22 (("ECB" 2 13))))
       70)
      ("Version"
       4
       (6 26)
       (("ECBlocks" 20 (("ECB" 1 80)))
        ("ECBlocks" 18 (("ECB" 2 32)))
        ("ECBlocks" 26 (("ECB" 2 24)))
        ("ECBlocks" 16 (("ECB" 4 9))))
       100)
      ("Version"
       5
       (6 30)
       (("ECBlocks" 26 (("ECB" 1 108)))
        ("ECBlocks" 24 (("ECB" 2 43)))
        ("ECBlocks" 18 (("ECB" 2 15) ("ECB" 2 16)))
        ("ECBlocks" 22 (("ECB" 2 11) ("ECB" 2 12))))
       134)
      ("Version"
       6
       (6 34)
       (("ECBlocks" 18 (("ECB" 2 68)))
        ("ECBlocks" 16 (("ECB" 4 27)))
        ("ECBlocks" 24 (("ECB" 4 19)))
        ("ECBlocks" 28 (("ECB" 4 15))))
       172)
      ("Version"
       7
       (6 22 38)
       (("ECBlocks" 20 (("ECB" 2 78)))
        ("ECBlocks" 18 (("ECB" 4 31)))
        ("ECBlocks" 18 (("ECB" 2 14) ("ECB" 4 15)))
        ("ECBlocks" 26 (("ECB" 4 13) ("ECB" 1 14))))
       196)
      ("Version"
       8
       (6 24 42)
       (("ECBlocks" 24 (("ECB" 2 97)))
        ("ECBlocks" 22 (("ECB" 2 38) ("ECB" 2 39)))
        ("ECBlocks" 22 (("ECB" 4 18) ("ECB" 2 19)))
        ("ECBlocks" 26 (("ECB" 4 14) ("ECB" 2 15))))
       242)
      ("Version"
       9
       (6 26 46)
       (("ECBlocks" 30 (("ECB" 2 116)))
        ("ECBlocks" 22 (("ECB" 3 36) ("ECB" 2 37)))
        ("ECBlocks" 20 (("ECB" 4 16) ("ECB" 4 17)))
        ("ECBlocks" 24 (("ECB" 4 12) ("ECB" 4 13))))
       292)
      ("Version"
       10
       (6 28 50)
       (("ECBlocks" 18 (("ECB" 2 68) ("ECB" 2 69)))
        ("ECBlocks" 26 (("ECB" 4 43) ("ECB" 1 44)))
        ("ECBlocks" 24 (("ECB" 6 19) ("ECB" 2 20)))
        ("ECBlocks" 28 (("ECB" 6 15) ("ECB" 2 16))))
       346)
      ("Version"
       11
       (6 30 54)
       (("ECBlocks" 20 (("ECB" 4 81)))
        ("ECBlocks" 30 (("ECB" 1 50) ("ECB" 4 51)))
        ("ECBlocks" 28 (("ECB" 4 22) ("ECB" 4 23)))
        ("ECBlocks" 24 (("ECB" 3 12) ("ECB" 8 13))))
       404)
      ("Version"
       12
       (6 32 58)
       (("ECBlocks" 24 (("ECB" 2 92) ("ECB" 2 93)))
        ("ECBlocks" 22 (("ECB" 6 36) ("ECB" 2 37)))
        ("ECBlocks" 26 (("ECB" 4 20) ("ECB" 6 21)))
        ("ECBlocks" 28 (("ECB" 7 14) ("ECB" 4 15))))
       466)
      ("Version"
       13
       (6 34 62)
       (("ECBlocks" 26 (("ECB" 4 107)))
        ("ECBlocks" 22 (("ECB" 8 37) ("ECB" 1 38)))
        ("ECBlocks" 24 (("ECB" 8 20) ("ECB" 4 21)))
        ("ECBlocks" 22 (("ECB" 12 11) ("ECB" 4 12))))
       532)
      ("Version"
       14
       (6 26 46 66)
       (("ECBlocks" 30 (("ECB" 3 115) ("ECB" 1 116)))
        ("ECBlocks" 24 (("ECB" 4 40) ("ECB" 5 41)))
        ("ECBlocks" 20 (("ECB" 11 16) ("ECB" 5 17)))
        ("ECBlocks" 24 (("ECB" 11 12) ("ECB" 5 13))))
       581)
      ("Version"
       15
       (6 26 48 70)
       (("ECBlocks" 22 (("ECB" 5 87) ("ECB" 1 88)))
        ("ECBlocks" 24 (("ECB" 5 41) ("ECB" 5 42)))
        ("ECBlocks" 30 (("ECB" 5 24) ("ECB" 7 25)))
        ("ECBlocks" 24 (("ECB" 11 12) ("ECB" 7 13))))
       655)
      ("Version"
       16
       (6 26 50 74)
       (("ECBlocks" 24 (("ECB" 5 98) ("ECB" 1 99)))
        ("ECBlocks" 28 (("ECB" 7 45) ("ECB" 3 46)))
        ("ECBlocks" 24 (("ECB" 15 19) ("ECB" 2 20)))
        ("ECBlocks" 30 (("ECB" 3 15) ("ECB" 13 16))))
       733)
      ("Version"
       17
       (6 30 54 78)
       (("ECBlocks" 28 (("ECB" 1 107) ("ECB" 5 108)))
        ("ECBlocks" 28 (("ECB" 10 46) ("ECB" 1 47)))
        ("ECBlocks" 28 (("ECB" 1 22) ("ECB" 15 23)))
        ("ECBlocks" 28 (("ECB" 2 14) ("ECB" 17 15))))
       815)
      ("Version"
       18
       (6 30 56 82)
       (("ECBlocks" 30 (("ECB" 5 120) ("ECB" 1 121)))
        ("ECBlocks" 26 (("ECB" 9 43) ("ECB" 4 44)))
        ("ECBlocks" 28 (("ECB" 17 22) ("ECB" 1 23)))
        ("ECBlocks" 28 (("ECB" 2 14) ("ECB" 19 15))))
       901)
      ("Version"
       19
       (6 30 58 86)
       (("ECBlocks" 28 (("ECB" 3 113) ("ECB" 4 114)))
        ("ECBlocks" 26 (("ECB" 3 44) ("ECB" 11 45)))
        ("ECBlocks" 26 (("ECB" 17 21) ("ECB" 4 22)))
        ("ECBlocks" 26 (("ECB" 9 13) ("ECB" 16 14))))
       991)
      ("Version"
       20
       (6 34 62 90)
       (("ECBlocks" 28 (("ECB" 3 107) ("ECB" 5 108)))
        ("ECBlocks" 26 (("ECB" 3 41) ("ECB" 13 42)))
        ("ECBlocks" 30 (("ECB" 15 24) ("ECB" 5 25)))
        ("ECBlocks" 28 (("ECB" 15 15) ("ECB" 10 16))))
       1085)
      ("Version"
       21
       (6 28 50 72 94)
       (("ECBlocks" 28 (("ECB" 4 116) ("ECB" 4 117)))
        ("ECBlocks" 26 (("ECB" 17 42)))
        ("ECBlocks" 28 (("ECB" 17 22) ("ECB" 6 23)))
        ("ECBlocks" 30 (("ECB" 19 16) ("ECB" 6 17))))
       1156)
      ("Version"
       22
       (6 26 50 74 98)
       (("ECBlocks" 28 (("ECB" 2 111) ("ECB" 7 112)))
        ("ECBlocks" 28 (("ECB" 17 46)))
        ("ECBlocks" 30 (("ECB" 7 24) ("ECB" 16 25)))
        ("ECBlocks" 24 (("ECB" 34 13))))
       1258)
      ("Version"
       23
       (6 30 54 78 102)
       (("ECBlocks" 30 (("ECB" 4 121) ("ECB" 5 122)))
        ("ECBlocks" 28 (("ECB" 4 47) ("ECB" 14 48)))
        ("ECBlocks" 30 (("ECB" 11 24) ("ECB" 14 25)))
        ("ECBlocks" 30 (("ECB" 16 15) ("ECB" 14 16))))
       1364)
      ("Version"
       24
       (6 28 54 80 106)
       (("ECBlocks" 30 (("ECB" 6 117) ("ECB" 4 118)))
        ("ECBlocks" 28 (("ECB" 6 45) ("ECB" 14 46)))
        ("ECBlocks" 30 (("ECB" 11 24) ("ECB" 16 25)))
        ("ECBlocks" 30 (("ECB" 30 16) ("ECB" 2 17))))
       1474)
      ("Version"
       25
       (6 32 58 84 110)
       (("ECBlocks" 26 (("ECB" 8 106) ("ECB" 4 107)))
        ("ECBlocks" 28 (("ECB" 8 47) ("ECB" 13 48)))
        ("ECBlocks" 30 (("ECB" 7 24) ("ECB" 22 25)))
        ("ECBlocks" 30 (("ECB" 22 15) ("ECB" 13 16))))
       1588)
      ("Version"
       26
       (6 30 58 86 114)
       (("ECBlocks" 28 (("ECB" 10 114) ("ECB" 2 115)))
        ("ECBlocks" 28 (("ECB" 19 46) ("ECB" 4 47)))
        ("ECBlocks" 28 (("ECB" 28 22) ("ECB" 6 23)))
        ("ECBlocks" 30 (("ECB" 33 16) ("ECB" 4 17))))
       1706)
      ("Version"
       27
       (6 34 62 90 118)
       (("ECBlocks" 30 (("ECB" 8 122) ("ECB" 4 123)))
        ("ECBlocks" 28 (("ECB" 22 45) ("ECB" 3 46)))
        ("ECBlocks" 30 (("ECB" 8 23) ("ECB" 26 24)))
        ("ECBlocks" 30 (("ECB" 12 15) ("ECB" 28 16))))
       1828)
      ("Version"
       28
       (6 26 50 74 98 122)
       (("ECBlocks" 30 (("ECB" 3 117) ("ECB" 10 118)))
        ("ECBlocks" 28 (("ECB" 3 45) ("ECB" 23 46)))
        ("ECBlocks" 30 (("ECB" 4 24) ("ECB" 31 25)))
        ("ECBlocks" 30 (("ECB" 11 15) ("ECB" 31 16))))
       1921)
      ("Version"
       29
       (6 30 54 78 102 126)
       (("ECBlocks" 30 (("ECB" 7 116) ("ECB" 7 117)))
        ("ECBlocks" 28 (("ECB" 21 45) ("ECB" 7 46)))
        ("ECBlocks" 30 (("ECB" 1 23) ("ECB" 37 24)))
        ("ECBlocks" 30 (("ECB" 19 15) ("ECB" 26 16))))
       2051)
      ("Version"
       30
       (6 26 52 78 104 130)
       (("ECBlocks" 30 (("ECB" 5 115) ("ECB" 10 116)))
        ("ECBlocks" 28 (("ECB" 19 47) ("ECB" 10 48)))
        ("ECBlocks" 30 (("ECB" 15 24) ("ECB" 25 25)))
        ("ECBlocks" 30 (("ECB" 23 15) ("ECB" 25 16))))
       2185)
      ("Version"
       31
       (6 30 56 82 108 134)
       (("ECBlocks" 30 (("ECB" 13 115) ("ECB" 3 116)))
        ("ECBlocks" 28 (("ECB" 2 46) ("ECB" 29 47)))
        ("ECBlocks" 30 (("ECB" 42 24) ("ECB" 1 25)))
        ("ECBlocks" 30 (("ECB" 23 15) ("ECB" 28 16))))
       2323)
      ("Version"
       32
       (6 34 60 86 112 138)
       (("ECBlocks" 30 (("ECB" 17 115)))
        ("ECBlocks" 28 (("ECB" 10 46) ("ECB" 23 47)))
        ("ECBlocks" 30 (("ECB" 10 24) ("ECB" 35 25)))
        ("ECBlocks" 30 (("ECB" 19 15) ("ECB" 35 16))))
       2465)
      ("Version"
       33
       (6 30 58 86 114 142)
       (("ECBlocks" 30 (("ECB" 17 115) ("ECB" 1 116)))
        ("ECBlocks" 28 (("ECB" 14 46) ("ECB" 21 47)))
        ("ECBlocks" 30 (("ECB" 29 24) ("ECB" 19 25)))
        ("ECBlocks" 30 (("ECB" 11 15) ("ECB" 46 16))))
       2611)
      ("Version"
       34
       (6 34 62 90 118 146)
       (("ECBlocks" 30 (("ECB" 13 115) ("ECB" 6 116)))
        ("ECBlocks" 28 (("ECB" 14 46) ("ECB" 23 47)))
        ("ECBlocks" 30 (("ECB" 44 24) ("ECB" 7 25)))
        ("ECBlocks" 30 (("ECB" 59 16) ("ECB" 1 17))))
       2761)
      ("Version"
       35
       (6 30 54 78 102 126 150)
       (("ECBlocks" 30 (("ECB" 12 121) ("ECB" 7 122)))
        ("ECBlocks" 28 (("ECB" 12 47) ("ECB" 26 48)))
        ("ECBlocks" 30 (("ECB" 39 24) ("ECB" 14 25)))
        ("ECBlocks" 30 (("ECB" 22 15) ("ECB" 41 16))))
       2876)
      ("Version"
       36
       (6 24 50 76 102 128 154)
       (("ECBlocks" 30 (("ECB" 6 121) ("ECB" 14 122)))
        ("ECBlocks" 28 (("ECB" 6 47) ("ECB" 34 48)))
        ("ECBlocks" 30 (("ECB" 46 24) ("ECB" 10 25)))
        ("ECBlocks" 30 (("ECB" 2 15) ("ECB" 64 16))))
       3034)
      ("Version"
       37
       (6 28 54 80 106 132 158)
       (("ECBlocks" 30 (("ECB" 17 122) ("ECB" 4 123)))
        ("ECBlocks" 28 (("ECB" 29 46) ("ECB" 14 47)))
        ("ECBlocks" 30 (("ECB" 49 24) ("ECB" 10 25)))
        ("ECBlocks" 30 (("ECB" 24 15) ("ECB" 46 16))))
       3196)
      ("Version"
       38
       (6 32 58 84 110 136 162)
       (("ECBlocks" 30 (("ECB" 4 122) ("ECB" 18 123)))
        ("ECBlocks" 28 (("ECB" 13 46) ("ECB" 32 47)))
        ("ECBlocks" 30 (("ECB" 48 24) ("ECB" 14 25)))
        ("ECBlocks" 30 (("ECB" 42 15) ("ECB" 32 16))))
       3362)
      ("Version"
       39
       (6 26 54 82 110 138 166)
       (("ECBlocks" 30 (("ECB" 20 117) ("ECB" 4 118)))
        ("ECBlocks" 28 (("ECB" 40 47) ("ECB" 7 48)))
        ("ECBlocks" 30 (("ECB" 43 24) ("ECB" 22 25)))
        ("ECBlocks" 30 (("ECB" 10 15) ("ECB" 67 16))))
       3532)
      ("Version"
       40
       (6 30 58 86 114 142 170)
       (("ECBlocks" 30 (("ECB" 19 118) ("ECB" 6 119)))
        ("ECBlocks" 28 (("ECB" 18 47) ("ECB" 31 48)))
        ("ECBlocks" 30 (("ECB" 34 24) ("ECB" 34 25)))
        ("ECBlocks" 30 (("ECB" 20 15) ("ECB" 61 16))))
       3706)))
  
  #| Version:getVersionNumber
version: the version to get the version number from
return: the version number for the version passed in
|#
  (defun Version:getVersionNumber (version)
    (nth *Version:VERSION_NUMBER* version))
  
  #| Version:getAlignmentPatternCenters
version: the version to get the alignment pattern centers from
return: the alignment pattern centers for the version passed in
|#
  (defun Version:getAlignmentPatternCenters (version)
    (nth *Version:ALIGNMENT_PATTERN_CENTERS* version))
  
  #| Version:getTotalCodewords
version: the version to get the Total codewords for
return: the total codewords for the version passed in
|#
  (defun Version:getTotalCodewords (version)
    (nth *Version:TOTAL_CODE_WORDS* version))
  
  #| Version:getDimensionForVersion
version: the version to get the dimension for
return: the dimension for the version passed in
|#
  (defun Version:getDimensionForVersion (version)
    (+ 17 (* 4 (Version:getVersionNumber version))))
  
  #| Version:getECBlocksforLevel
version: the version to get the EC blocks for
ecLevel: the desired error correction level
return: the error correction blocks for the given version with the given ec level
|#
  (defun Version:getECBlocksForLevel (version ecLevel)
    (let* ((index (ErrorCorrectionLevel:ordinal ecLevel)))
      (nth index (nth *Version:EC_Blocks* version))))
  
  #| Version:getVersionForNumber
versionNumber: the version number to get the version for
return: a version type for the version number specified
|#
  (defun Version:getVersionForNumber (versionNumber)
    (nth (- versionNumber 1) *Version:VERSIONS*))
  
  #| Version:getProvisionalVersionForDimension
dimension: the dimension to get the provisional version for
return: the provisionalVersion associated with the dimension passed in
|#
  (defun Version:getProvisionalVersionForDimension (dimension)
    (Version:getVersionForNumber (>> (- dimension 17) 2)))
  
  #| Version:decodeVersionInformationLoop
versionBits: bits of the encoded version
bestDifference: the best difference of the versions checked so far
bestVersion: the version that got the best difference
i: loop iterator
return: decoded version information
|#
  (defun Version:decodeVersionInformationLoop 
    (versionBits bestDifference bestVersion i)
    (if (< i (length *Version:VERSION_DECODE_INFO*))
        (let* ((targetVersion (nth i *Version:VERSION_DECODE_INFO*)))
          (if (equal targetVersion versionBits)
              (Version:getVersionForNumber (+ i 7))
              (let* 
                  ((bitsDifference(FormatInformation:numBitsDiffering 
                                   versionBits targetVersion)))
                (if (< bitsDifference bestDifference)
                    (Version:decodeVersionInformationLoop 
                     versionBits bitsDifference (+ i 7) (+ i 1))
                    (Version:decodeVersionInformationLoop 
                     versionBits bestDifference bestVersion (+ i 1))))))
        (if (<= bestDifference 3)
            (Version:getVersionForNumber bestVersion)
            nil)))
  
  #| Version:decodeVersionInformation
versionBits: the version to decode
return: the version information
|#
  (defun Version:decodeVersionInformation (versionBits)
    (let* ((bestDifference 2147483647)
           (bestVersion 0))
      (Version:decodeVersionInformationLoop 
       versionBits bestDifference bestVersion 0)))
  
  #| Version:buildFunctionPatternLoop1
version: vension to build function with
dimension: dimension to use for building the function pattern
bm: the byte matrix used for the function
max: length of the alignment pattern centers
x: iterator for the x coords
y: iterator for the y coords
return: built function
|#
  (defun Version:buildFunctionPatternLoop1 (version dimension bm max x y)
    (let* ((alignment (nth *Version:ALIGNMENT_PATTERN_CENTERS* version)))
      (if (< x max)
          (let* ((i (- (nth x alignment) 2)))
            (if (< y max)
                (if (or (and (equal x 0) (or (equal y 0) (equal y (- max 1))))
                        (and (equal x (- max 1)) (equal y 0)))
                    (Version:buildFunctionPatternLoop1 
                     version dimension bm max x (+ y 1))
                    (let* ((newbm 
                            (BitMatrix:setRegion 
                             bm (- (nth y alignment) 2) i 5 5)))
                      (Version:buildFunctionPatternLoop1 
                       version dimension newbm max x (+ y 1))))          
                (Version:buildFunctionPatternLoop1 
                 version dimension bm max (+ x 1) 0)))
          (let* ((vertical (BitMatrix:setRegion bm 6 9 1 (- dimension 17)))
                 (horizontal (BitMatrix:setRegion 
                              vertical 9 6 (- dimension 17) 1)))
            (if (> (nth *Version:VERSION_NUMBER* version) 6)
                (let* ((topRight (BitMatrix:setRegion 
                                  horizontal (- dimension 11) 0 3 6))
                       (bottomLeft(BitMatrix:setRegion 
                                   topRight 0 (- dimension 11) 6 3)))
                  bottomLeft)
                horizontal)))))
  
  #| Version:buildFunctionPattern
version: the version to build the function pattern around
return: the function pattern based on the version passed in
|#
  (defun Version:buildFunctionPattern (version)
    (let* ((dimension (Version:getDimensionForVersion version))
           (bitMatrix (BitMatrix$1 dimension))
           (topLeft (BitMatrix:setRegion bitMatrix 0 0 9 9))
           (topRight (BitMatrix:setRegion topLeft (- dimension 8) 0 8 9))
           (bottomLeft (BitMatrix:setRegion topRight 0 (- dimension 8) 9 8))
           (max (length (nth *Version:ALIGNMENT_PATTERN_CENTERS* version))))
      (Version:buildFunctionPatternLoop1 
       version dimension bottomLeft max 0 0)))
  
  #| Mode
characterCountBitsForVersion: The number of characters in bits for the version
bits: the bits for the mode
name: the name of the mode
return: gives back a Mode data structure with the paramaters inserted
|#
  (defun Mode (characterCountBitsForVersions bits name)
    (list "Mode" characterCountBitsForVersions bits name))
  
  (defconst *Mode:TERMINATOR* (Mode (list 0 0 0) 0 "TERMINATOR"))
  (defconst *Mode:NUMERIC* (Mode (list 10 12 14) 1 "NUMERIC"))
  (defconst *Mode:ALPHANUMERIC* (Mode (list 9 11 13) 2 "ALPHANUMERIC"))
  (defconst *Mode:STRUCTURED_APPEND* (Mode (list 0 0 0) 3 "STRUCTURED_APPEND"))
  (defconst *Mode:BYTE* (Mode (list 8 16 16) 4 "BYTE"))
  (defconst *Mode:ECI* (Mode nil 7 "ECI"))
  (defconst *Mode:KANJI* (Mode (list 8 10 12) 8 "KANJI"))
  (defconst *Mode:FNC1_FIRST_POSITION* (Mode nil 5 "FNC1_FIRST_POSITION"))
  (defconst *Mode:FNC1_SECOND_POSITION* (Mode nil 9 "FNC1_SECOND_POSITION"))
  (defconst *Mode:FORBITS* (list *Mode:TERMINATOR* *Mode:NUMERIC* *Mode:ALPHANUMERIC* *Mode:STRUCTURED_APPEND* *Mode:BYTE* *Mode:FNC1_FIRST_POSITION* nil *Mode:ECI* *Mode:KANJI* *Mode:FNC1_SECOND_POSITION*))
  
  (defconst *Mode:CHARACTER_COUNT_BITS_FOR_VERSIONS* 1)
  (defconst *Mode:BITS* 2)
  (defconst *Mode:NAME* 3)
  
  #| Mode:forBits
bits: the index in Mode:FORBITS to get
return: the Mode at the index specified
|#
  (defun Mode:forBits (bits)
    (nth bits *Mode:FORBITS*))
  
  #| Mode:getBits
mode: the mode to get the bits for
return: the bits of the mode passed in
|#
  (defun Mode:getBits(mode)
    (nth *Mode:BITS* mode))
  
  #| Mode:getName
mode: the mode to get the name for
return: the name of the mode passed in
|#
  (defun Mode:getName (mode)
    (nth *Mode:NAME* mode))
  
  #| Mode:getCharacterCountBits
mode: the mode to get the character count bits for
version: which version to consider when getting the character count bits
return: the charactercount bits for the mode with the given version
|#
  (defun Mode:getCharacterCountBits (mode version)
    (let* ((characterCountBitsForVersions (nth *Mode:CHARACTER_COUNT_BITS_FOR_VERSIONS* mode))
           (number (Version:getVersionNumber version))
           (offset (if (<= number 9) 0
                       (if (<= number 26) 1 2))))
      (nth offset characterCountBitsForVersions)))
  
  (export IQRCode))