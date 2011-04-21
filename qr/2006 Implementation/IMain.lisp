;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
(interface IMain
  (sig encode (text ecLevel filename topng))
  (sig EC:L ())
  (sig EC:M ())
  (sig EC:Q ())
  (sig EC:H ()))