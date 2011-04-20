;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
(interface IIO
  (sig read-text-file (filename state))
  (sig read-bitmap-file (filename state))
  (sig write-text-file (filename data state))
  (sig write-bitmap-file (filename data state))
  )