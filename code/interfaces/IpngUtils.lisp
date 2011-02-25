(interface IpngUtils
  (sig blowChunks (pngdata))
  (sig makeChunk (chunktype chunkdata))
  (sig makeNum (num flag))
  (sig parseNum (string flag))
  (sig calcCRC32 (string))

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
)
