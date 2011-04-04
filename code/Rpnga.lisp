;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")

#| Team Steele
   Software Engineering II
   Rpnga
  
   Main Linking and Invoking File
|#

(require "modules/Mbasiclex.lisp")
(require "modules/MapngBuilder.lisp")
(require "modules/MapngExploder.lisp")
(require "modules/MminidomParser.lisp")
(require "modules/Mio.lisp")
(require "modules/MminidomSerializer.lisp")
(require "modules/MxmlUtils.lisp")
(require "modules/MpngUtils.lisp")


(link Rpnga 
      (Mbasiclex MminidomParser MminidomSerializer MpngUtils MxmlUtils
                 MapngExploder MapngBuilder Mio))

(invoke Rpnga)

(set-state-ok t)

;(time$ (animate "testpngs/test" state)) 
;(time$ (animate "rickrollframes/test" state)) 
;  cpu time: 677650 real time: 678763 gc time: 17784 (for avl-tree crc32)
;  cpu time: 631743 real time: 632529 gc time: 39424 (for no precalc)
;  for 212 frames
;(suspend "testexplode/test" state)
