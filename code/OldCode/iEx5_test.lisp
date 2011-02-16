#| Edward Flick
   Software Engineering I
   iEx5_test

   Tests for the iEx5 assignment.
|#

(in-package "ACL2")
(include-book "testing" :dir :teachpacks)
(include-book "doublecheck" :dir :teachpacks)
(include-book "iEx5")

; Removed all tests from iEx4 to keep this clean
; Only testing new functions

; getresults (xmlnodes prefix postfix)
(check-expect (getresults nil *tablehead* *tabletail*)
              (string-append *tablehead* *tabletail*))
(check-expect
 (getresults
  (list
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
   ))
  *tablehead*
  *tabletail*)
 (stringlist-append
  (list
   *tablehead*
   "<tr>"
   "<td>A3</td>"
   "<td>A5</td>"
   "<td>5.0</td><td>&nbsp;</td>"
   "<td>120.0</td>"
   "<td>120.0</td>"
   "</tr>"
   *tabletail*)))
(check-expect
 (getresults
  (list
   (mv
    "Result"
    (list
     (mv "SectionLabel" "A")
     (mv "PairID-NS" "3")
     (mv "PairID-EW" "5")
     )
    (list
     (mv "TotalScore"
         (list (mv "Direction" "E-W"))
         (list (mv 'text nil "7.0"))
         )
     (mv "MatchpointsNS" nil (list (mv 'text nil "120.0")))
     (mv "MatchpointsEW" nil (list (mv 'text nil "120.0")))
     )
   ))
  *tablehead*
  *tabletail*)
 (stringlist-append
  (list
   *tablehead*
   "<tr>"
   "<td>A3</td>"
   "<td>A5</td>"
   "<td>&nbsp;</td><td>7.0</td>"
   "<td>120.0</td>"
   "<td>120.0</td>"
   "</tr>"
   *tabletail*)))

; getboards (xmlnode)
(check-expect (getboards nil) "")
(check-expect
 (getboards
  (list
   (mv
    "Board"
    nil
    (list
     (mv "Vulnerable" nil (list (mv 'text nil "NS")))
     (mv "Dealer" nil (list (mv 'text nil "N")))
     (mv "BoardNo" nil (list (mv 'text nil "33")))
     (mv "Deal" nil
         (list
          (mv 
           "Hand"
           (list (mv "direction" "N"))
           (list
            (mv "Suit"
                (list (mv "symbol" "S"))
                (list (mv 'text nil "234")))
            ))
          )))))
  )
 (stringlist-append (list
  "<div class=\"board\">"
  "<div class=\"boardnum\">Board: 33</div>\n"
  "<div class=\"N\">"
  "<div class=\"dealer\">Dealer</div>\n"
  "<div class=\"vulnerable\">Vulnerable</div>\n"
  "&spades;234<br />\n"
  "</div>\n"  
  "</div>\n"
  *tablehead*
  *tabletail*)))
(check-expect
 (getboards
  (list
   (mv
    "Board"
    nil
    (list
     (mv "Vulnerable" nil (list (mv 'text nil "NS")))
     (mv "Dealer" nil (list (mv 'text nil "N")))
     (mv "BoardNo" nil (list (mv 'text nil "33")))
     (mv "Deal" nil
         (list
          (mv 
           "Hand"
           (list (mv "direction" "N"))
           (list
            (mv "Suit"
                (list (mv "symbol" "S"))
                (list (mv 'text nil "234")))
            ))
          ))
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
     )))
  )
 (stringlist-append (list
  "<div class=\"board\">"
  "<div class=\"boardnum\">Board: 33</div>\n"
  "<div class=\"N\">"
  "<div class=\"dealer\">Dealer</div>\n"
  "<div class=\"vulnerable\">Vulnerable</div>\n"
  "&spades;234<br />\n"
  "</div>\n"  
  "</div>\n"
  *tablehead*
  "<tr>"
  "<td>A3</td>"
  "<td>A5</td>"
  "<td>5.0</td><td>&nbsp;</td>"
  "<td>120.0</td>"
  "<td>120.0</td>"
  "</tr>"
  *tabletail*)))
