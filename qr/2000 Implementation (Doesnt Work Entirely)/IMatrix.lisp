;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
(interface IMatrix
  (sig empty-matrix? (matrix))
  (sig empty-matrix ())
  (sig get-width (matrix))
  (sig get-height (matrix))
  (sig mset (matrix row col val))
  (sig mget (matrix row col))
  (sig get-square (matrix tlRow tlCol brRow brCol))
  (sig get-row (matrix row col))
  (sig flatten-matrix (matrix)))