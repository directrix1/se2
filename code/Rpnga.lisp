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
(require "modules/MinidomSerializer.lisp")
(require "modules/MxmlUtils.lisp")
(require "modules/MpngUtils.lisp")

(link Rbridge 
      (Mbasiclex MapngBuilder MapngExploder MminidomParser MminidomSerializer
                 Mio MxmlUtils MpngUtils))

(invoke Rpnga)

(set-state-ok t)

