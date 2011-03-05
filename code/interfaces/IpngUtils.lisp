;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#| Team Steele
   Software Engineering I
   Ipngutils

   Functions to assist with parsing and constructing PNG files.
|#
(in-package "ACL2")

(interface IpngUtils
  ; Returns the crc32 lookup table value for a given index.
  ;  index = the key for the value to lookup
  (sig crc32Lookup (index))
  
  ; Given a previously calculated crc32 value and raw data bytes, such as
  ; that found in the data portion of a PNG Image chunk, returns an updated
  ; CRC value based on the new bytes.
  ;  crc32 = previously computed CRC32
  ;  bytes = raw data from PNG Image or other source
  (sig updateCRC32 (crc32 bytes))
  
  ; Given raw data bytes, such as that found in the data portion of a
  ; PNG Image chunk, returns the calculated CRC.
  ;  bytes = raw data from PNG Image or other source
  (sig calcCRC32 (bytes))

  ; Converts the given number into an unsigned int or other options to be
  ; used in chunk processing / creating.
  ;  num = number to be converted 
  ;  signed = true means make it two's complement
  ;  numbytes = number of bytes used in representation
  (sig makeNum (num signed numbytes))
  
  ; Parses a number given in a non-standard type.
  ;  bytes = number to be parsed
  ;  signed = true means make it two's complement
  ;  numbytes = number of bytes used in representation
  (sig parseNum (bytes signed numbytes))
  
  ; Turns an ascii string into it's equivalent in bytes.
  ;  string = a string containing only ascii characters
  (sig ascii->bytes (string))
  
  ; Turns ascii bytes into it's equivalent string.
  ;  bytes = a list of bytes that represent only ascii characters
  (sig bytes->ascii (bytes))  
  
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
  (sig blowChunks (pngdata))
  
  ; Given a chunk type and correctly formatted chunkdata, makeChunk returns
  ; the correctly formatted chunk including the chunk length, type, data,
  ; and CRC (using calcCRC32).
  ;  chunktype = type of the chunk to be created, a length 4 ascii string
  ;  chunkdata = raw data portion of the chunk to be created as byte list
  (sig makeChunk (chunktype chunkdata))
  
  
  (con makeNum-inverts-parseNum
       (implies 
        (and
         (natp numbytes)
         (natp number)
         (< number (ash 1 (* 8 numbytes)))
         )
        (let 
            ((signednumber (- number (ash 1 (- (* 8 numbytes) 1)))))
          (and
           (= (parseNum (makeNum signednumber t numbytes) t numbytes) signednumber)
           (= (parseNum (makeNum number nil numbytes) nil numbytes) number)))))
  #|
  (con blowChunks-returns-at-least-IHDR-IDAT
	(implies (stringp pngdat)
		 (let ((chunksBlown (blowChunks pngdat)))
		 	(and (eql (caar chunksBlown) "IHDR")
			   (or (eql (second (car chunksBlown) "IDAT"))
			      (eql (fourth (car chunksblown) "IDAT")))))))

 (con makeChunk-returns-string
	(implies (and (stringp chunktype)
		      (stringp chunkdat)
		(let ((chunked (makeChunk (chunktype chunkdat))))
		     (stringp chunked)))))

 (con makeNum-returns-unsigned-int
	(implies (acl2-numberp number)
		 (acl2-numberp flag)
		(let ((uns-int (makeNum number flag)))
		     ;something here to test - no idea
		)))
 |#
)
