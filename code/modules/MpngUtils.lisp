;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#| Team Steele
   Software Engineering I
   Mpngutils

   Functions to assist with parsing and constructing PNG files.
|#
(in-package "ACL2")

(require "../interfaces/IpngUtils.lisp")
(require "../interfaces/Ibasiclex.lisp")

(module MpngUtils
  (include-book "list-utilities" :dir :teachpacks)
  (include-book "avl-rational-keys" :dir :teachpacks)
  
  ; Helper function to convert the crc32 lookup table to an avl-tree with
  ; the key being it's index-based position in the list.
  ;  index = the current key
  ;  alist = the list of values to be associated with each key
  (defun list->avl (index alist)
    (if (null alist)
        (empty-tree)
        (avl-insert
         (list->avl (+ 1 index) (cdr alist))
         index
         (car alist))))
  
  (defconst
    *crc32Lookup* (list->avl 0
    '(#x00000000 #x77073096 #xee0e612c #x990951ba #x076dc419 #x706af48f 
      #xe963a535 #x9e6495a3 #x0edb8832 #x79dcb8a4 #xe0d5e91e #x97d2d988 
      #x09b64c2b #x7eb17cbd #xe7b82d07 #x90bf1d91 #x1db71064 #x6ab020f2 
      #xf3b97148 #x84be41de #x1adad47d #x6ddde4eb #xf4d4b551 #x83d385c7 
      #x136c9856 #x646ba8c0 #xfd62f97a #x8a65c9ec #x14015c4f #x63066cd9 
      #xfa0f3d63 #x8d080df5 #x3b6e20c8 #x4c69105e #xd56041e4 #xa2677172 
      #x3c03e4d1 #x4b04d447 #xd20d85fd #xa50ab56b #x35b5a8fa #x42b2986c 
      #xdbbbc9d6 #xacbcf940 #x32d86ce3 #x45df5c75 #xdcd60dcf #xabd13d59 
      #x26d930ac #x51de003a #xc8d75180 #xbfd06116 #x21b4f4b5 #x56b3c423 
      #xcfba9599 #xb8bda50f #x2802b89e #x5f058808 #xc60cd9b2 #xb10be924 
      #x2f6f7c87 #x58684c11 #xc1611dab #xb6662d3d #x76dc4190 #x01db7106 
      #x98d220bc #xefd5102a #x71b18589 #x06b6b51f #x9fbfe4a5 #xe8b8d433 
      #x7807c9a2 #x0f00f934 #x9609a88e #xe10e9818 #x7f6a0dbb #x086d3d2d 
      #x91646c97 #xe6635c01 #x6b6b51f4 #x1c6c6162 #x856530d8 #xf262004e 
      #x6c0695ed #x1b01a57b #x8208f4c1 #xf50fc457 #x65b0d9c6 #x12b7e950 
      #x8bbeb8ea #xfcb9887c #x62dd1ddf #x15da2d49 #x8cd37cf3 #xfbd44c65 
      #x4db26158 #x3ab551ce #xa3bc0074 #xd4bb30e2 #x4adfa541 #x3dd895d7 
      #xa4d1c46d #xd3d6f4fb #x4369e96a #x346ed9fc #xad678846 #xda60b8d0 
      #x44042d73 #x33031de5 #xaa0a4c5f #xdd0d7cc9 #x5005713c #x270241aa 
      #xbe0b1010 #xc90c2086 #x5768b525 #x206f85b3 #xb966d409 #xce61e49f 
      #x5edef90e #x29d9c998 #xb0d09822 #xc7d7a8b4 #x59b33d17 #x2eb40d81 
      #xb7bd5c3b #xc0ba6cad #xedb88320 #x9abfb3b6 #x03b6e20c #x74b1d29a 
      #xead54739 #x9dd277af #x04db2615 #x73dc1683 #xe3630b12 #x94643b84 
      #x0d6d6a3e #x7a6a5aa8 #xe40ecf0b #x9309ff9d #x0a00ae27 #x7d079eb1 
      #xf00f9344 #x8708a3d2 #x1e01f268 #x6906c2fe #xf762575d #x806567cb 
      #x196c3671 #x6e6b06e7 #xfed41b76 #x89d32be0 #x10da7a5a #x67dd4acc 
      #xf9b9df6f #x8ebeeff9 #x17b7be43 #x60b08ed5 #xd6d6a3e8 #xa1d1937e 
      #x38d8c2c4 #x4fdff252 #xd1bb67f1 #xa6bc5767 #x3fb506dd #x48b2364b 
      #xd80d2bda #xaf0a1b4c #x36034af6 #x41047a60 #xdf60efc3 #xa867df55 
      #x316e8eef #x4669be79 #xcb61b38c #xbc66831a #x256fd2a0 #x5268e236 
      #xcc0c7795 #xbb0b4703 #x220216b9 #x5505262f #xc5ba3bbe #xb2bd0b28 
      #x2bb45a92 #x5cb36a04 #xc2d7ffa7 #xb5d0cf31 #x2cd99e8b #x5bdeae1d 
      #x9b64c2b0 #xec63f226 #x756aa39c #x026d930a #x9c0906a9 #xeb0e363f 
      #x72076785 #x05005713 #x95bf4a82 #xe2b87a14 #x7bb12bae #x0cb61b38 
      #x92d28e9b #xe5d5be0d #x7cdcefb7 #x0bdbdf21 #x86d3d2d4 #xf1d4e242 
      #x68ddb3f8 #x1fda836e #x81be16cd #xf6b9265b #x6fb077e1 #x18b74777 
      #x88085ae6 #xff0f6a70 #x66063bca #x11010b5c #x8f659eff #xf862ae69 
      #x616bffd3 #x166ccf45 #xa00ae278 #xd70dd2ee #x4e048354 #x3903b3c2 
      #xa7672661 #xd06016f7 #x4969474d #x3e6e77db #xaed16a4a #xd9d65adc 
      #x40df0b66 #x37d83bf0 #xa9bcae53 #xdebb9ec5 #x47b2cf7f #x30b5ffe9 
      #xbdbdf21c #xcabac28a #x53b39330 #x24b4a3a6 #xbad03605 #xcdd70693 
      #x54de5729 #x23d967bf #xb3667a2e #xc4614ab8 #x5d681b02 #x2a6f2b94 
      #xb40bbe37 #xc30c8ea1 #x5a05df1b #x2d02ef8d)))
    
  ; Returns the crc32 lookup table value for a given index.
  ;  index = the key for the value to lookup
  (defun crc32Lookup (index)
    (cdr (avl-retrieve *crc32Lookup* index)))

  ; Given a previously calculated crc32 value and raw data bytes, such as
  ; that found in the data portion of a PNG Image chunk, returns an updated
  ; CRC value based on the new bytes.
  ;  crc32 = previously computed CRC32
  ;  bytes = raw data from PNG Image or other source
  (defun updateCRC32 (crc32 bytes)
    (if (null bytes)
        crc32
        (updateCRC32
         (logxor
          (crc32Lookup (logand (logxor crc32 (car bytes)) #xff))
          (ash crc32 -8))
         (cdr bytes))))
  
  ; Given raw data bytes, such as that found in the data portion of a
  ; PNG Image chunk, returns the calculated CRC.
  ;  bytes = raw data from PNG Image or other source
  (defun calcCRC32 (bytes)
    (logxor (updateCRC32 #xffffffff bytes) #xffffffff))
  
  ; Converts the given number into an unsigned int or other options to be
  ; used in chunk processing / creating.
  ;  num = number to be converted 
  ;  signed = true means make it two's complement
  ;  numbytes = number of bytes used in representation
  (defun makeNum (num signed numbytes)
    (if (zp numbytes)
        nil
        (let ((thenum
               (if (and signed (< num 0))
                   (+ (ash 1 (* 8 numbytes)) num)
                   num)))
          (cons 
           (ash thenum (* 8 (- 1 numbytes)))
           (makeNum (logand num (- (ash 1 (* 8 (- numbytes 1))) 1)) nil (- numbytes 1))))))
  
  ; Parses a number given in a non-standard type.
  ;  bytes = number to be parsed
  ;  signed = true means make it two's complement
  ;  numbytes = number of bytes used in representation
  (defun parseNum (bytes signed numbytes)
    (if (zp numbytes)
        0
        (let ((thenum (+ 
                       (ash (car bytes) (* 8 (- numbytes 1)))
                       (parseNum (cdr bytes) nil (- numbytes 1)))))
          (if (and signed (> (car bytes) 127))
              (- thenum (ash 1 (* 8 numbytes)))
              thenum))))

  ; Helper function that turns a list of chars into a list of bytes.
  ;  chars = the list of chars
  (defun chars->bytes (chars)
    (if (null chars)
        nil
        (cons (char-code (car chars))
              (chars->bytes (cdr chars)))))

  ; Helper function that turns a list of bytes into a list of chars.
  ;  bytes = the list of bytes
  (defun bytes->chars (bytes)
    (if (null bytes)
        nil
        (cons (code-char (car bytes))
              (bytes->chars (cdr bytes)))))

  ; Turns an ascii string into it's equivalent in bytes.
  ;  string = a string containing only ascii characters
  (defun ascii->bytes (string)
    (chars->bytes (coerce string 'list)))
  
  ; Turns ascii bytes into it's equivalent string.
  ;  bytes = a list of bytes that represent only ascii characters
  (defun bytes->ascii (bytes)
    (coerce (bytes->chars bytes) 'string))

  ; After being given a lot of PNG image data, blowChunks processes this
  ; data on a chunk by chunk basis and subsequently returns the list of
  ; list pairs of chunk type (ascii string) and chunk data (byte list).
  ; This function drops any chunk with an invalid crc32.
  ;	For example:
  ;	*PNG Image → 	(  (list IHDR ihdr_data) (list IDAT idat_data) 
  ;					... 			..	)
  ;	*APNG Image → 	(  (list IHDR ihdr_data) (list acTL actl_data) 
  ;			   	(list fcTL fctl_1)    (list fdAT fdat_1)
  ;				(list fcTL fctl_2)    (list fdAT fdat_2)
  ;					...		        ..	)
  ;	pngdata = raw, unprocessed png data bytes
  (defun blowChunks (pngdata)
    (if (< (len pngdata) 12)        ; Enough for length, type, and crc
        nil
        (let*
            (
             (chunklen (parseNum (take 4 pngdata) nil 4))
             (rem1 (nthcdr 4 pngdata))
             (chunktype (take 4 rem1))
             (rem2 (nthcdr 4 rem1))
             (chunkdata (take chunklen rem2))
             (rem3 (nthcdr chunklen rem2))
             (crc (parseNum (take 4 rem3) nil 4))
             (remainder (nthcdr 4 rem3))
             )
          ; If the CRC is not equal then we drop the chunk
          (if (= (calcCRC32 (concatenate 'list chunktype chunkdata)) crc)
              (cons (list (bytes->ascii chunktype) chunkdata)
                    (blowChunks remainder))
              (blowChunks remainder)))))
         
  
  ; Given a chunk type and correctly formatted chunkdata, makeChunk returns
  ; the correctly formatted chunk including the chunk length, type, data,
  ; and CRC (using calcCRC32).
  ;  chunktype = type of the chunk to be created, a length 4 ascii string
  ;  chunkdata = raw data portion of the chunk to be created as byte list
  (defun makeChunk (chunktype chunkdata)
    (let ((chunktypebytes (ascii->bytes chunktype)))
    (concatenate 'list
                 (makeNum (len chunkdata) nil 4)
                 chunktypebytes
                 chunkdata
                 (makeNum
                  (calcCRC32 (concatenate 'list chunktypebytes chunkdata))
                  nil 4))))
      
(export IpngUtils))