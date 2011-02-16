#| Edward Flick
   Software Engineering I
   iEx5_pred

   Predicate tests for the iEx5 assignment.
|#

(in-package "ACL2")
(include-book "testing" :dir :teachpacks)
(include-book "doublecheck" :dir :teachpacks)
(include-book "iEx5")

; Removed all tests from iEx4 to keep this clean
; Only testing new functions

(defproperty getresults-nil=string-append-prefix-postfix-tst :repeat 100
  (prefix :value (random-string)
   postfix :value (random-string))
  (implies (and (stringp prefix) (stringp postfix))
           (string-equal (getresults nil prefix postfix)
                         (string-append prefix postfix)))
)

(defproperty getresults-results-proportional-to-input-tst :repeat 100
  (n     :value (random-between 1 200)
   nodes :value (random-list-of
                 (mv
                  "Result"
                  (list
                   (mv "SectionLabel" "A")
                   (mv "PairID-NS" "3")
                   (mv "PairID-EW" "5")
                   )
                  (list
                   (mv "TotalScore"
                       (list (mv "Direction" "N-S"))
                       (list (mv 'text nil "5.0"))
                       )
                   (mv "MatchpointsNS" nil (list (mv 'text nil "120.0")))
                   (mv "MatchpointsEW" nil (list (mv 'text nil "120.0")))
                   )
                  )
                 :size n))
  (implies (> n 0)
           (< (length (getresults (cdr nodes) "" ""))
              (length (getresults nodes "" ""))))
)
