;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.ss" ("cce" "dracula.plt") "modular" "lang")
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
			(stringp (writeXML numPlays numFrames framedata)))))
)
