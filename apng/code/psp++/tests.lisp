(in-package "ACL2")

(include-book "date-time")
(include-book "doublecheck" :dir :teachpacks)
(include-book "testing" :dir :teachpacks)

(check-expect (parse-date "Jan 1, 1980") 0)

(check-expect (split2 (str->chrs "Testing!") nil #\e) 
              '("T" (#\s #\t #\i #\n #\g #\!)))
(check-expect (split2 (str->chrs "Testing!") nil #\@) 
              '("Testing!" nil))
(check-expect (split2 (str->chrs "") nil #\@) 
              '("" nil))

(check-expect (possible-key '(#\t #\e #\s #\t)) nil)
(check-expect (possible-key '(#\T #\E #\S #\T)) t)
(check-expect (possible-key '(#\t)) nil)
(check-expect (possible-key '(#\T)) t)
(check-expect (possible-key nil) t)

(check-expect (bar-split '(#\t #\e #\s #\t #\| #\i #\n #\g #\| #\!)) 
              '("test" "ing" "!"))
(check-expect (bar-split '(#\t #\e #\s #\t)) 
              '("test"))
(check-expect (bar-split nil) 
              nil)

(check-expect (line-replace "Name is |NAME|" '(("NAME" . "Ben")))
              "Name is Ben")
(check-expect (template-replace nil nil) nil)
(check-expect (template-replace nil '("test" "test2")) nil)

(check-expect (drop-all '(1) '(1 2 3 1 2 3 1)) '(2 3 1 2 3 1))
(check-expect (drop-all '(1 1 1) '(1 2 3 1 2 3 1)) '(2 3 1 2 3 1))
(check-expect (drop-all '(1 2 3) '(1 2 3 1 2 3 1)) nil)
(check-expect (drop-all nil '(1 2 3 1 2 3 1)) '(1 2 3 1 2 3 1))

(check-expect (drop-all-tail '(1) '(1 2 3 1 2 3 1) '(1 2 3)) 
              '(1 1 2 3 2 3 1 2 3))
(check-expect (drop-all-tail '(1) '(1 2 3 1 2 3 1) '(1)) 
              '(1 1 2 3 1 2 3))

(check-expect (trim (str->chrs "     Test    ")) (str->chrs "Test"))
(check-expect (trim (str->chrs "Test\t")) (str->chrs "Test"))
(check-expect (trim (str->chrs "\tTest")) (str->chrs "Test"))
(check-expect (trim (str->chrs "         ")) (str->chrs ""))
(check-expect (trim (str->chrs "")) (str->chrs ""))

(check-expect (split-on-whitespace "Test test2\ttest3" '(#\space)) 
              '("Test" "test2\ttest3"))
(check-expect (split-on-whitespace "Test test2\ttest3" '(#\space #\tab)) 
              '("Test" "test2" "test3"))

(check-expect (rat-strp (str->chrs "5")) t)
(check-expect (rat-strp (str->chrs "5.0")) t)
(check-expect (rat-strp (str->chrs "124.235")) t)
(check-expect (rat-strp (str->chrs "-124.235")) t)
(check-expect (rat-strp (str->chrs "+5.2")) nil)
(check-expect (rat-strp (str->chrs "abba")) nil)

(check-expect (string-concat '("a" "b" "c")) "abc")
(check-expect (string-concat '("a")) "a")
(check-expect (string-concat nil) "")
(check-expect (string-concat '("The " "quick " "red " "fox " "jumped " 
                                      "over " "the " "lazy " "brown " 
                                      "dog."))
              "The quick red fox jumped over the lazy brown dog.")

(check-expect (str-list->chrs-list '("Test" "Test2")) 
              '((#\T #\e #\s #\t) (#\T #\e #\s #\t #\2)))
(check-expect (str-list->chrs-list nil) nil)
(check-expect (str-list->chrs-list '("Test")) '((#\T #\e #\s #\t)))

(defproperty date-format-parse-relation
  (date :value (random-between 0 10000))
  (= (parse-date (format-date date))
     date))
(check-expect (parse-date "Jan 1, 1980") 0)
(check-expect (parse-date "Dec 10, 2009") 10936)

(defproperty time-format-parse-relation
  (time :value (random-between 0 (1- (* 24 60))))
  (= (parse-time (format-time time))
     time))

(check-expect (parse-time "12:00 AM") 0) ;midnight
(check-expect (parse-time "1:01 AM") 61)
(check-expect (parse-time "12:00 PM") (* 60 12)) ;noon