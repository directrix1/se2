(include-book "operations")

; Constructs a line object
(defun make-line (item-beginner key value line-number line-text)
  (list item-beginner (string-downcase key) value line-number line-text))
; Accessor for the boolean is-item-beginner field of a line
(defun is-item-beginner (line)
  (first line))
; Accessor for the string key field of a line
(defun get-key (line)
  (second line))
; Accessor for the string value field of a line
(defun get-value (line)
  (third line))
; Accessor for the natp line-numberfield of a line
(defun get-line-number (line)
  (fourth line))
; Accessor for the string line-text field of a line
(defun get-line-text (line)
  (fifth line))

; parse-line - converts a string into a line object. A line is 
; formatted as
; <key>:<value>#comment
; The # and 'comment' field is optional. Whitespace between any of 
; the tokens listed will be ignored.
;
; return value: 
;  If an error occurs, returns a string describing that error
;  If this line is content-less, returns nil
;  If this line is normal, returns a line structure
(defun parse-line (line line-number)
  (let* ((comment-stripped (take-to #\# line))
         (key-value (break-at #\: comment-stripped))
         (dash-key (trim (first key-value)))
         (dash-exists (and (consp dash-key) (equal (first dash-key) 
                                                   #\-)))
         (key (if dash-exists
                  (trim (rest dash-key))
                  dash-key))
         (colon-value (second key-value))
         (value (if (consp colon-value)
                    (trim (cdr colon-value))
                    nil)))
    (if (endp colon-value)
        (if (endp dash-key)
            nil ; Empty line; OK
            "Expected a colon") ; Missing colon
        (if (endp key)
            "Missing a key before the colon"
            (make-line dash-exists 
                       (chrs->str key) 
                       (chrs->str value) 
                       line-number 
                       (chrs->str line))))))

; parse-lines - parses each string in a list as a line.
; 
; line-number :: nat - line number of the first element in the list.
; lines :: [string] - lines to parse
;
; If an error occurs in parsing any line, returns a string describing 
; that error. Otherwise, returns a list of lines.
(defun parse-lines (line-number lines)
  (if (endp lines) nil
      (let ((parse (parse-line (str->chrs (first lines)) line-number)))
        (if (stringp parse) ; test for error
            (string-concat (list "Error on line "
                                 (rat->str line-number 0)
                                 " ("
                                 (first lines)
                                 ") : "
                                 parse))
            (let ((other-parses (parse-lines (1+ line-number) 
                                             (rest lines))))
              (if (stringp other-parses)
                  other-parses
                  (if (null parse) other-parses
                      (cons parse other-parses))))))))

; Schema for the data files. List-openers are lists and actual key-values
; are strings.
(defun aschema ()
  '("name" 
    "program"
    "language"
    "date"
    "instructor"
    "actual added lines"
    "actual base lines"
    "actual modified lines"
    "actual removed lines"
    ("new objects" "name" 
                   "type"
                   "estimated lines"
                   "comment")
    ("reused objects" "name" 
                      "type"
                      "estimated base"
                      "estimated removed"
                      "estimated modified"
                      "estimated added"
                      "comment")
    ("time log" "date"
                "phase"
                "start time"
                "end time"
                "interruption"
                "comment")
    ("defect log" "date"
                  "type"
                  "fix time"
                  "comment")))

; Look up the given key in the given schema. Only the top level of the
; schema is searched.
; 
; key :: string - key to look up
; schema :: schema to search
;
; returns schema or string
(defun find-schema-element (key schema)
  (if (endp schema)
      nil
      (let ((element (first schema)))
        (if (string-equal key
                          (if (consp element)
                              (first element)
                              element))
            element
            (find-schema-element key (rest schema))))))

; Parses a sequence of lines according to the given schema until it 
; reaches a key that is unrecognized.
;
; so-far :: line structure - parsed structure so far
; line-parses - [line] - list of lines to parse
; schema - schema to parse based on
;
; returns a list (parsed-structure leftover-lines)
(defun parse-structure-rec (so-far line-parses schema)
  (if (endp line-parses)
      (list (reverse so-far) nil)
      (let* ((line-parse (first line-parses))
             (schema-element (find-schema-element (get-key line-parse) 
                                                  schema)))
        (if (null schema-element)
            (list (reverse so-far) line-parses)
            (if (stringp schema-element)
                (parse-structure-rec (cons line-parse
                                           so-far)
                                     (rest line-parses)
                                     schema)
                (let* ((rec-call (parse-structure-rec nil
                                                      (rest line-parses)
                                                      (rest 
                                                       schema-element)))
                       (lines-left (second rec-call))
                       (parsed-substructure (first rec-call)))
                  (parse-structure-rec (cons (list line-parse 
                                                   parsed-substructure)
                                             so-far)
                                       lines-left
                                       schema)))))))

; Parses a sequence of lines according to the given schema until it 
; reaches a key that is unrecognized.
;
; line-parses - [line] - list of lines to parse
; schema - schema to parse based on
;
; returns a list (parsed-structure leftover-lines)
(defun parse-structure (line-parses schema)
  (let* ((parse (parse-structure-rec nil line-parses schema))
         (parse-leftovers (second parse)))
    (if (endp parse-leftovers)
        (first parse)
        (string-concat (list "Unrecognized key on line "
                             (rat->str (get-line-number 
                                        (first parse-leftovers)) 0)
                             ": "
                             (get-key (first parse-leftovers)))))))

; count-key - Counts the number of top-level occurrences of a string 
; in a structure.
;
; returns natural
(defun count-key (structure key)
  (if (endp structure)
      0
      (+ (if (and (atom (first (first structure)))
                  (string-equal (get-key (first structure))
                                key))
             1
             0)
         (count-key (rest structure) key))))

; count-substructure - counts the number of top-level occurrences of a 
; substructure in a structure.
;
; returns natural
(defun count-substructure (structure key)
  (if (endp structure)
      0
      (+ (if (and (not (atom (first (first structure))))
                  (string-equal (get-key (first (first structure)))
                                key))
             1
             0)
         (count-substructure (rest structure) key))))

; get-attr :: gets the line corresponding to the given key in the given
; structure. If no such key exists, the value is considered ot be nil
;
; structure - structure to search it
; key :: string - key to look for
;
; returns line or nil
(defun get-attr (structure key)
  (if (endp structure) 
      nil
      (if (and (atom (first (first structure)))
               (string-equal (get-key (first structure))
                             key))
          (first structure)
          (get-attr (rest structure) key))))

; get-attr-value :: gets the value corresponding to the given key in the
; given structure.
;
; structure - structure to search it
; key :: string - key to look for
;
; returns string or nil
(defun get-attr-value (structure key)
  (let ((attr (get-attr structure key)))
    (if (endp attr) nil
        (get-value (get-attr structure key)))))

; get-date-value :: gets the value, interpreted as a date corresponding 
; to the given key in the given structure.
;
; structure - structure to search it
; key :: string - key to look for
;
; returns string or nil
(defun get-date-value (structure key)
  (let ((attr (get-attr structure key)))
    (if (endp attr)
        nil
        (let ((parsed (parse-date (get-value attr))))
          (if (stringp parsed)
              (string-concat (list "Invalid date on line "
                                   (rat->str (get-line-number attr) 0)
                                   ": "
                                   parsed))
              parsed)))))

; get-int-value :: gets the value, interpreted as a int corresponding 
; to the given key in the given structure.
;
; structure - structure to search it
; key :: string - key to look for
;
; returns string or nil
(defun get-int-value (structure key)
  (let ((attr (get-attr structure key)))
    (if (endp attr)
        nil
        (if (rat-strp (str->chrs (get-value attr)))
            (str->rat (get-value attr))
            (string-concat (list "Expected an integer on line "
                                 (rat->str (get-line-number attr) 0)
                                 ", but got "
                                 (get-value attr)))))))

; get-time-value :: gets the value, interpreted as a time corresponding 
; to the given key in the given structure.
;
; structure - structure to search it
; key :: string - key to look for
;
; returns string or nil
(defun get-time-value (structure key)
  (let ((attr (get-attr structure key)))
    (if (endp attr)
        nil
        (let ((parsed (parse-time (get-value attr))))
          (if (null parsed)
              (string-concat (list "Invalid time on line "
                                   (rat->str (get-line-number attr) 0)
                                   ": "
                                   parsed))
              parsed)))))

; get-substructure - extracts a substructure labeled by the given key 
; from the given structure.
;
; structure - structure to search it
; key :: string - key to look for
;
; returns structure or nil
(defun get-substructure (structure key)
  (if (endp structure) 
      nil
      (if (and (not (atom (first (first structure))))
               (string-equal (get-key (first (first structure)))
                             key))
          (if (string-equal (get-value (first (first structure)))
                            "")
              (first (rest (first structure)))
              (string-concat 
               (list "Line " 
                     (rat->str 
                      (get-line-number (first (first structure))) 0)    
                     ": There should be no value following the colon in "
                     (get-line-text (first (first structure))))))
          (get-substructure (rest structure) key))))


(defun ensure-counts (constraints structure)
  (if (endp constraints)
      nil
      (let* ((constraint (first constraints))
             (type (first constraint))
             (name (second constraint))
             (minimum (third constraint))
             (maximum (fourth constraint))
             (count (if (equal type 'attr)
                        (count-key structure name)
                        (count-substructure structure name))))
        (if (and (or (null maximum)
                     (<= count maximum))
                 (or (null minimum)
                     (>= count minimum)))
            (ensure-counts (rest constraints) structure)
            (string-append "Wrong number of " name))))) ; TODO: make 
                                                        ;  this a nice 
                                                        ;  error message

; Divides a structure into items delimited by dashes.
; note: assumes that parses startes with a is-item-beginner
;
; parses - [line] :: lines to delimit
; current - [line] :: initially nil. Reversed elements in current item
;
; returns [[line]]
(defun dash-divide (parses current)
  (if (endp parses)
      (reverse current)
      (if (is-item-beginner (first parses))
          (dash-divide (rest parses)
                       (cons (list (first parses))
                             current))
          (dash-divide (rest parses)
                       (cons (reverse (cons (first parses) 
                                            (first current)))
                             (rest current))))))

; Parses a single defect structure
; returns defect or error string
(defun parse-single-defect (structure)
  (let ((count-error (ensure-counts (list (list 'attr "date" 1 1)
                                          (list 'attr "type" 1 1)
                                          (list 'attr "fix time" 1 1)
                                          (list 'attr "comment" 0 1))
                                    structure)))
    (if (stringp count-error) 
        count-error
        (let ((date (get-date-value structure "date"))
              (fix-time (get-int-value structure "fix time")))
          (cond ((stringp date) date)
                ((stringp fix-time) fix-time)
                (t (make-defect date
                                (get-attr-value structure "type")
                                fix-time
                                (get-attr-value structure 
                                                "comment"))))))))

; Parses a single time log structure
; returns time log error string
(defun parse-single-time-log (structure)
  (let ((count-error (ensure-counts (list (list 'attr "date" 1 1)
                                          (list 'attr "phase" 1 1)
                                          (list 'attr "start time" 1 1)
                                          (list 'attr "end time" 1 1)
                                          (list 'attr "interruption" 0 1)
                                          (list 'attr "comment" 0 1))
                                    structure)))
    (if (stringp count-error) 
        count-error
        (let ((date (get-date-value structure "date"))
              (interruption (get-int-value structure "interruption"))
              (start-time (get-time-value structure "start time"))
              (end-time (get-time-value structure "end time")))
          (cond ((stringp date) date)
                ((stringp interruption) interruption)
                ((stringp start-time) start-time)
                ((stringp end-time) end-time)
                (t
                 (make-time-log date
                                (get-attr-value structure "phase")
                                start-time
                                (if (< start-time end-time)
                                    (- end-time start-time)
                                    (+ end-time (- start-time) 
                                       (* 24 60)))
                                interruption
                                (get-attr-value structure 
                                                "comment"))))))))

; Parses a single reused object structure
; returns code-object or error string
(defun parse-single-reused-object (structure)
  (let ((count-error (ensure-counts (list (list 'attr "type" 1 1)
                                          (list 'attr 
                                                "estimated base" 1 1)
                                          (list 'attr 
                                                "estimated removed" 0 1)
                                          (list 'attr 
                                                "estimated modified" 0 1)
                                          (list 'attr 
                                                "estimated added" 0 1)
                                          (list 'attr "comment" 0 1))
                                    structure)))
    (if (stringp count-error) 
        count-error
        (let ((base (get-int-value structure "estimated base"))
              (removed (get-int-value structure "estimated removed"))
              (modified (get-int-value structure "estimated modified"))
              (added (get-int-value structure "estimated added")))
          (cond ((stringp base) base)
                ((stringp removed) removed)
                ((stringp modified) modified)
                ((stringp added) added)
                (t (make-code-object (get-attr-value structure "name")
                                     (get-attr-value structure "type")
                                     base
                                     removed
                                     modified
                                     added
                                     (get-attr-value structure 
                                                     "comment"))))))))

; Parses a single new object structure
; returns code-object or error string
(defun parse-single-new-object (structure)
  (let ((count-error (ensure-counts (list (list 'attr "name" 1 1)
                                          (list 'attr "type" 1 1)
                                          (list 'attr "estimated lines" 
                                                1 1)
                                          (list 'attr "comment" 0 1))
                                    structure)))
    (if (stringp count-error) 
        count-error
        (let ((lines (get-int-value structure "estimated lines")))
          (cond ((stringp lines) lines)
                (t (make-code-object (get-attr-value structure "name")
                                     (get-attr-value structure "type")
                                     0
                                     0
                                     0
                                     lines
                                     (get-attr-value structure
                                                     "comment"))))))))


; Helper function for parsing a list of new objects
; returns [code-object] or error string
(defun parse-new-objects-rec (structures)
  (if (endp structures)
      nil
      (let ((first-parsed (parse-single-new-object (first structures)))
            (rest-parsed (parse-new-objects-rec (rest structures))))
        (cond ((stringp first-parsed) first-parsed)
              ((stringp rest-parsed) rest-parsed)
              (t  (cons first-parsed
                        rest-parsed))))))

; Parses a list of new objects
; returns [code-object] or error string
(defun parse-new-objects (structure)
  (if (endp structure)
      nil
      (if (is-item-beginner (first structure))
          (let ((divided (dash-divide structure nil)))
            (parse-new-objects-rec divided))
          "Each new object should start with a dash.")))

; Helper function for parsing a list of defects
; returns [defect] or error string
(defun parse-defects-rec (structures)
  (if (endp structures)
      nil
      (let ((first-parsed (parse-single-defect (first structures)))
            (rest-parsed (parse-defects-rec (rest structures))))
        (cond ((stringp first-parsed) first-parsed)
              ((stringp rest-parsed) rest-parsed)
              (t  (cons first-parsed
                        rest-parsed))))))

; Parses a list of defects
; returns [defect] or error string
(defun parse-defects (structure)
  (if (endp structure)
      nil
      (if (is-item-beginner (first structure))
          (let ((divided (dash-divide structure nil)))
            (parse-defects-rec divided))
          "Each defect entry should start with a dash.")))

; Helper function for parsing a list of time log entries
; returns [time-log] or error string
(defun parse-time-logs-rec (structures)
  (if (endp structures)
      nil
      (let ((first-parsed (parse-single-time-log (first structures)))
            (rest-parsed (parse-time-logs-rec (rest structures))))
        (cond ((stringp first-parsed) first-parsed)
              ((stringp rest-parsed) rest-parsed)
              (t  (cons first-parsed
                        rest-parsed))))))

; Parses a list of time logs
; returns [time-log] or error string
(defun parse-time-logs (structure)
  (if (endp structure)
      nil
      (if (is-item-beginner (first structure))
          (let ((divided (dash-divide structure nil)))
            (parse-time-logs-rec divided))
          "Each time log entry should start with a dash.")))

; Helper function for parsing a list of reused objects
; returns [code-object] or error string
(defun parse-reused-objects-rec (structures)
  (if (endp structures)
      nil
      (let ((first-parsed (parse-single-reused-object 
                           (first structures)))
            (rest-parsed (parse-reused-objects-rec (rest structures))))
        (cond ((stringp first-parsed) first-parsed)
              ((stringp rest-parsed) rest-parsed)
              (t  (cons first-parsed 
                        rest-parsed))))))

; Parses a list of reused objects
; returns [code-object] or error string
(defun parse-reused-objects (structure)
  (if (endp structure)
      nil
      (if (is-item-beginner (first structure))
          (let ((divided (dash-divide structure nil)))
            (parse-reused-objects-rec divided))
          "Each reused object should start with a dash.")))

; Parses a user-data object out of the given structure. This is 
;  the top-level parse.
(defun parse-user-data (structure)
  (let ((count-error (ensure-counts (list (list 'attr "name" 0 1)
                                          (list 'attr "program" 1 1)
                                          (list 'attr "instructor" 0 1)
                                          (list 'attr "date" 0 1)
                                          (list 'attr 
                                                "actual added lines" 0 1)
                                          (list 'attr 
                                                "actual base lines" 0 1)
                                          (list 'attr 
                                                "actual modified lines"
                                                0 1)
                                          (list 'attr 
                                                "actual removed lines"
                                                0 1)
                                          (list 'substructure 
                                                "new objects" 0 1)
                                          (list 'substructure 
                                                "reused objects" 0 1)
                                          (list 'substructure 
                                                "defect log" 0 1)
                                          (list 'substructure 
                                                "time log" 0 1))
                                    structure)))
    (if (stringp count-error)
        count-error
        (let* ((date (get-date-value structure "date"))
               (actual-added (get-int-value structure 
                                            "actual added lines"))
               (actual-base (get-int-value structure 
                                           "actual base lines"))
               (actual-modified (get-int-value structure 
                                               "actual modified lines"))
               (actual-removed (get-int-value structure 
                                              "actual removed lines"))
               (attributes (cond ((stringp date) date)
                                 ((stringp actual-added) actual-added)
                                 ((stringp actual-base) actual-base)
                                 ((stringp actual-modified)
                                  actual-modified)
                                 ((stringp actual-removed)
                                  actual-removed)
                                 (t (make-attributes (get-attr-value 
                                                      structure "name")
                                                     (get-attr-value 
                                                      structure 
                                                      "program")
                                                     (get-attr-value 
                                                      structure 
                                                      "instructor")
                                                     date
                                                     actual-added
                                                     actual-base
                                                     actual-modified
                                                     actual-removed
                                                     (get-attr-value 
                                                      structure 
                                                      "language")))))
               (new-objects-struct (get-substructure structure 
                                                     "new objects"))
               (new-objects (if (stringp new-objects-struct) 
                                new-objects-struct
                                (parse-new-objects new-objects-struct)))
               (reused-objects-struct 
                (get-substructure structure "reused objects"))
               (reused-objects (if (stringp reused-objects-struct)
                                   reused-objects-struct
                                   (parse-reused-objects 
                                    reused-objects-struct)))
               (defect-logs-struct (get-substructure structure 
                                                     "defect log"))
               (defect-log (if (stringp defect-logs-struct) 
                               defect-logs-struct
                               (parse-defects defect-logs-struct)))
               (time-log-struct (get-substructure structure "time log"))
               (time-log (if (stringp time-log-struct)
                             time-log-struct
                             (parse-time-logs time-log-struct))))
          (cond ((stringp attributes) attributes)
                ((stringp new-objects) new-objects)
                ((stringp reused-objects) reused-objects)
                ((stringp defect-log) defect-log)
                ((stringp time-log) time-log)
                (t (make-user-data attributes
                                   (append new-objects reused-objects)
                                   defect-log
                                   time-log)))))))

; Parses a list of strings representing a file into a user-data 
;  structure.
; Each line is of the format 
;   -? key: value #comment
; Some keys indicate the opening of sections, such as "New objects".
;  These sections contain a list of elements, delimited by dashes.
;
; lines :: [string]
; return user-data or error string
(defun parse-file (lines)
  (let* ((line-parses (parse-lines 1 lines))
         (structure-parse (if (stringp line-parses)
                              line-parses
                              (parse-structure line-parses (aschema))))
         (user-data (if (stringp structure-parse)
                        structure-parse
                        (parse-user-data structure-parse))))
    user-data))