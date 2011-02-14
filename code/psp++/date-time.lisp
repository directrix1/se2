(include-book "string-suggest")

; Association list of month names onto their sequence number
(defun get-month-number-mapping ()
  (list '("January" . 1) '("Jan" . 1)
        '("February" . 2) '("Feb" . 2)
        '("March" . 3) '("Mar" . 3)
        '("April" . 4) '("Apr" . 4)
        '("May" . 5)
        '("June" . 6) '("Jun" . 6)
        '("July" . 7) '("Jul" . 7)
        '("August" . 8) '("Aug" . 8)
        '("September" . 9) '("Sept" . 9) '("Sep" . 9)
        '("October" . 10) '("Oct" . 10)
        '("November" . 11) '("Nov" . 11)
        '("December" . 12) '("Dec" . 12)))

; Number of days in each month on non-leap year years
(defun get-month-lengths ()
  (list 31 28 31 30 31 30 31 31 30 31 30 31))

; Number of days in each month on leap years
(defun get-leapyear-month-lengths ()
  (list 31 29 31 30 31 30 31 31 30 31 30 31))

; Month names to print, in order
(defun get-printed-month-names ()
  (list "Jan" "Feb" "March" "April" "May" "June" "July" "Aug" "Sept" 
        "Oct" "Nov" "Dec"))

; Month names to accept. Corresponds to keys in get-month-number-mapping
(defun get-month-names ()
  (list "January" "Jan"
        "February" "Feb"
        "March" "Mar"
        "April" "Apr"
        "May"
        "June" "Jun"
        "July" "Jul"
        "August" "Aug"
        "September" "Sept" "Sep"
        "October" "Oct"
        "November" "Nov"
        "December" "Dec"))

; Calculates the number of days since 1980 for the given date
;
; year :: nat - 4-digit year
; month :: nat - 1 corresponds to January
; day :: nat - day of the month, starting with 1
;
; returns nat
(defun days-since-1980 (year month day)
  (let* ((leap-days (floor (+ year -1980 (if (< 2 month) 4 3)) 4))
         (month-totals '(0 31 59 90 120 151 181 212 243 273 304 334))
         (days-in-earlier-years (+ (* 365 (+ year -1980))
                                   leap-days))
         (days-in-earlier-months (nth (1- month) month-totals)))
    (+ days-in-earlier-years days-in-earlier-months (1- day))))

; Checks that the given day-of-the-month is within bounds for the given
; month and year.
;
; returns boolean
(defun day-in-range (year month day)
  (and (<= 1 day)
       (<= day (+ (if (and (= month 2) (= (mod year 4) 0)) 1 0)
                  (nth (1- month) (get-month-lengths))))))

; Parses the given date, formatted as <month> <day> <year>, where 
; <month> is the text represention of the month and day and year
; are digits.
;
; If an error in parsing occurs, it will return a string describing 
; the error. Otherwise, it will return a natural number representing
; the number of days since 1980.
;
; returns integer or string
(defun parse-date (str)
  (let ((chunks (split-on-whitespace  str '(#\space #\tab #\, #\.))))
    (cond ((not (= (len chunks) 3)) 
 "The date should include month, day, and year. (For example, Nov 27, 2009.)")
          ((not (rat-strp (str->chrs (second chunks))))
           (string-concat (list 
      "The second part of a date should be the day of the month. Instead, "
                                (second chunks)
                                " was supplied.")))
          ((not (rat-strp (str->chrs (third chunks))))
           (string-concat (list
        "The third part of a date should be the four-digit year. Instead, "
                                (third chunks)
                                " was supplied.")))
          (t (let ((month (cdr (assoc-equal (first chunks) 
                                            (get-month-number-mapping))))
                   (year (str->rat (third chunks)))
                   (day (str->rat (second chunks))))
               (cond ((null month)
                      (string-concat (list "Invalid month provided: "
                                           (first chunks)
                                           ". Did you mean "
                                           (make-suggestion 
                                            (first chunks)                 
                                            (get-month-names))
                                           "?")))
                     ((or (< year 1980) (> year 2100))
                      (string-concat (list "Year out of range: " 
                                           (third chunks))))
                     ((not (day-in-range year month day))
                      (string-concat (list "Invalid day of the month: "
                                           (second chunks))))
                     (t (days-since-1980 year month day))))))))

; digitp - tests if the given character is a digit
; 
; chr :: character - character to test
; returns boolean
(defun digitp (chr)
  (let ((code (char-code chr)))
    (and (<= 48 code)
         (<= code 57))))

; only-digits - tests if all characters in the given list are digits
; only-digits cs = and (map digitp cs)
;
; chrs :: [character] - list of characters to test
; returns boolean
(defun only-digits (chrs)
  (if (endp chrs)
      nil
      (if (digitp (car chrs))
          (cons (car chrs)
                (only-digits (rest chrs)))
          (only-digits (rest chrs)))))

; parse-time - parses the given string, formatted as HH:MM <AM|PM> 
; as a time.
; If an error occurs, it returns a string describing that error. 
; Otherwise, the number of minutes since midnight is returned.
;
; str :: string - string to parse
; returns nat or string
(defun parse-time (str)
  (let* ((chunks (str-list->chrs-list (split-on-whitespace 
                                       (string-downcase str) '(#\:))))
         (minute-of-day 
          (if (= (len chunks) 2)
              (let* ((a-found (member-equal #\a (second chunks)))
                     (p-found (member-equal #\p (second chunks)))
                     (hour (str->rat (chrs->str (only-digits 
                                                 (first chunks)))))
                     (minute (str->rat (chrs->str (only-digits 
                                                   (second chunks))))))
                (if (or (and a-found p-found)
                        (and (not a-found) (not p-found))
                        (< minute 0)
                        (<= 60 minute)
                        (< hour 1)
                        (< 12 hour))
                    nil
                    (+ (if (= hour 12) 0 (* 60 hour))
                       minute
                       (if a-found 0 720))
                    ))
              nil)))
    (if (null minute-of-day)
        (string-concat (list "Invalid time: "
                             str
                    ". Times should be formatted as HH:MM am or HH:MM pm"))
        minute-of-day)))

; format-time - formats a time, represented by number of minutes since
;               midnight, as a string of the form HH:MM <AM|PM>.
;
; time :: nat - number of minutes since midnight
; returns string
(defun format-time (time)
  (let* ((hours-minutes (mod time 720))
         (hours (floor hours-minutes 60))
         (minutes (mod hours-minutes 60))
         (hours-adj (if (= hours 0) 12 hours))
         (am-pm (if (< time 720) "AM" "PM")))
    (string-concat (list (rat->str hours-adj 0)
                         ":"
                         (if (< minutes 10) "0" "")
                         (rat->str minutes 0)
                         " "
                         am-pm))))

; month-and-day - calculates the month and day for a given day of the 
;                 year
;
; month-lengths :: [nat] - lengths of the months in the year to 
;                          calculate for
; month-names :: [string] - names of the months to use
; day-of-year :: nat - day of the year to calculate for
;
; returns string
(defun month-and-day (day-of-year month-lengths month-names)
  (if (<= day-of-year (first month-lengths))
      (string-concat (list (first month-names)
                           " "
                           (rat->str day-of-year 0)))
      (month-and-day (- day-of-year (first month-lengths))
                     (rest month-lengths)
                     (rest month-names))))

(defun format-date-ex (date include-year)
  (let* ((leap-years (floor (+ date 1095) 1461))
         (leap-years-occurred (floor (+ date 1401) 1461))
         (years-since-1980 (floor (- date leap-years-occurred) 365))
         (year (+ 1980 years-since-1980))
         (is-leap-year (= (mod year 4) 0))
         (days-into-year (+ 1 date (* years-since-1980 -365) 
                            (- leap-years)))
         (these-month-lengths (if is-leap-year 
                                  (get-leapyear-month-lengths) 
                                  (get-month-lengths)))
         (except-year (month-and-day days-into-year these-month-lengths 
                                     (get-printed-month-names))))
    (if include-year
        (string-concat (list except-year
                             ", "
                             (rat->str year 0)))
        except-year)))

(defun format-month-and-day (date)
  (format-date-ex date nil))

; format-date - formats a date, represented by days since 1980, as a 
; string of the form <month> <day>, <year>.
;
; dat :: nat - number of days since 1980
; returns string
(defun format-date (date)
  (format-date-ex date t))