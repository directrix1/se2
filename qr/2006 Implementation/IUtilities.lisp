;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
(interface IUtilities
  (sig put-nth (n v l))
  (sig createList (n v))
  (sig getBytes (content))
  (sig intTobin (from to num))
  (sig int->bin (num))
  (sig binToint (from to num))
  (sig bin->int (num))
  (sig normalizei (i))
  (sig << (num i))
  (sig >> (num i))
  (sig >>> (num i)))