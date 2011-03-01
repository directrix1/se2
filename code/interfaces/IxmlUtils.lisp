(interface IxmlUtils
  (sig parseXML (domXML))
  (sig getFrames (framelist))
  (sig writeXML (numPlays numFrames framedata))
  (sig writeFrames (frameData))

  (con parseXML-returns-three
	(implies (true-listp domXML)
		 (= (len (parseXML domXML)) 3)))

  (con writeXML-delivers-string
	(implies (and (and (stringp numPlays) (stringp numFrames))
		(true-lisp framedata)
			(stringp (writeXML numPlays numFrames framedata))))
)
