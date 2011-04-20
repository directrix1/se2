(in-package "ACL2")

(include-book "parse")
(include-book "statistics")

; reorder-certainty-data - reformats certainty data from
; ((low1 . high1) ... (lown . highn)) to
; ((low1 low2 ... lown) (high1 high2 ... highn))
;
; low-highs :: [(rational . rationa)]
; returns ([rational] [rational])
(defun reorder-certainty-data (low-highs)
  (if (consp low-highs)
      (let* ((rest-low-highs (reorder-certainty-data (rest low-highs)))
             (lows (first rest-low-highs))
             (highs (second rest-low-highs))
             (low (car (first low-highs)))
             (high (cdr (first low-highs))))
        (list (cons low lows)
              (cons high highs)))
      (list nil nil)))

; Creates a line graph based on the given certainty range data.
; low-highs :: [(rational . rationa)] - lower and upper certainty bounds
(defun get-certainty-line-graph (low-highs)
  (if (endp low-highs)
      "Not enough data to make a meaningful prediction"
      (let* (;(low-highs (get-certainty-intervals data))
             (data (reorder-certainty-data low-highs))
             (lows (first data))
             (highs (second data)))
        (line-graph (list (cons "Lower bound" lows)
                          (cons "Upper bound" highs))
                    (list "10%" "20%" "30%" "40%" "50%" "60%" "70%" 
                          "80%" "90%")
                    ""))))

; Formats a defect entry as an HTML row.
; returns string
(defun format-defect-entry (defect)
  (string-concat (list "<tr><td>"
                       (format-date (defect-date defect))
                       "</td><td>"
                       (defect-type defect)
                       "</td><td>"
                       (rat->str (defect-fix-time defect) 0)
                       "</td><td>"
                       (defect-comment defect)
                       "</td></tr>")))

; Formats a list of defects into HTML rows
; returns string
(defun map-format-defect-entry (defects)
  (if (consp defects)
      (cons (format-defect-entry (first defects))
            (map-format-defect-entry (rest defects)))
      nil))

; Creates the contents of a defect log table for the given user-data
; returns string
(defun defect-log-table (user-data)
  (string-concat (list "<tr><th>Date</th><th>Phase</th>"
             "<th>Fix Time</th><th width=\"50%\">Description</th></tr>"
                       (string-concat 
                        (map-format-defect-entry 
                         (user-data-defect-log user-data))))))

; Formats a time-log entry as a HTML table row
; returns string
(defun format-time-entry (time-log)
  (string-concat (list "<tr><td>"
                       (format-date (time-log-date time-log))
                       ", "
                       (format-time (time-log-start-time time-log))
                       " - "
                       (format-time (+ (time-log-start-time time-log)
                                       (time-log-delta-time time-log)))
                       "</td><td>"
                       (time-log-phase time-log)
                       "</td><td>"
                       (rat->str (time-log-interruption-time time-log) 0)
                       "</td><td>"
                       (time-log-comment time-log)
                       "</td></tr>")))

; Formats a list of time low entries into HTML rows
; returns string
(defun map-format-time-entry (time-logs)
  (if (consp time-logs)
      (cons (format-time-entry (first time-logs))
            (map-format-time-entry (rest time-logs)))
      nil))

; Creates the contents of a time log table for the given user-data
; returns string
(defun time-log-table (user-data)
  (string-concat (list "<tr><th>Date</th><th>Type</th>"
             "<th>Int. Time</th><th width=\"50%\">Description</th></tr>"
                       (string-concat 
                        (map-format-time-entry 
                         (user-data-time-log user-data))))))

; drop-from-datasets - removes all but the first n entries from each 
; dataset in the list of datasets provided. Datasets is formatted as
; (("dataSetName1" val1 val2 ... valm)
;  ("dataSetName2" val1 val2 ... valm)
;  ...
;  ("dataSetNameN" val1 val2 ... valm))
;
; The return value is formatted in the same way.
(defun take-from-datasets (n datasets)
  (if (endp datasets)
      nil
      (cons (take (1+ n) (first datasets))
            (take-from-datasets n (rest datasets)))))

; drop-from-datasets - removes the first n entries from each dataset
; in the list of datasets provided. Datasets is formatted as
; (("dataSetName1" val1 val2 ... valm)
;  ("dataSetName2" val1 val2 ... valm)
;  ...
;  ("dataSetNameN" val1 val2 ... valm))
;
; The return value is formatted in the same way.
(defun drop-from-datasets (n datasets)
  (if (endp datasets)
      nil
      (cons (let* ((dataset (first datasets))
                   (label (first dataset))
                   (remaining-data (nthcdr (1+ n) dataset)))
              (cons label remaining-data))
            (drop-from-datasets n (rest datasets)))))

; Generates HTML for a list of bar graphs, with 10 columns per bar graph.
; This is useful because having too many bars will result in some being
; off the image, and splitting it into different graphs avoids 
; that problem.
;
; Input is identical to bar-graph in mgraph
; returns HTML string
(defun bar-graph-stack (data range title)
  (if (<= (length range) 10)
      (bar-graph data range title)
      (string-append
       (bar-graph (take-from-datasets 10 data) (take 10 range) title)
       (string-append 
        "<br>"
        (bar-graph-stack (drop-from-datasets 10 data) (nthcdr 10 range) 
                         title)))))

; Breaks time log information into phases and days and returns it in bar
; graph HTML format.
(defun time-by-day-graph (userdata)
  (let* ((data-and-range (time-by-day userdata))
         (data (first data-and-range))
         (range (second data-and-range)))
    (if (null data)
        "<i>No data</i>"
        (bar-graph-stack data range ""))))

; Formats a single row in the actual-estimate table
; returns string
(defun format-actual-estimate-table-entry (entry)
  (string-concat (list "<tr><td>"
                       (first entry)
                       "</td><td>"
                       (rat->str (second entry) 0)
                       "</td><td>"
                       (rat->str (third entry) 0)
                       "</td></tr>")))

; Formats each actual-vs-estimation as a entry in an HTML table
; returns [string]
(defun map-format-actual-estimate-table-entry (entries)
  (if (consp entries)
      (cons (format-actual-estimate-table-entry (first entries))
            (map-format-actual-estimate-table-entry (rest entries)))
      nil))

; Creates a the table contents for actual and estimated project lines.
(defun actual-estimate-table (data)
  (string-concat 
   (list "<tr><th>Project</th><th>Estimate</th><th>Actual</th></tr>"
         (string-concat
          (map-format-actual-estimate-table-entry data)))))

; Converts several data types to a presentable string.
; Strings are preserved.
; Rationals are formatted with 0 decimal digits.
; Anything else (in particular nil) is converted to "---"
;
; field :: anything
; returns string
(defun prepare-field (field)
  (cond ((stringp field) field)
        ((rationalp field) (rat->str field 0))
        (t "---")))

; get-estimated-lines - extracts the estimated lines entries from
; the history list formatted by line-and-time-history.
; returns [rational]
(defun get-estimated-lines (history)
  (if (consp history)
      (cons (second (first history))
            (get-estimated-lines (rest history)))
      nil))

; get-actual-lines - extracts the actual lines entries from
; the history list formatted by line-and-time-history.
; returns [rational]
(defun get-actual-lines (history)
  (if (consp history)
      (cons (third (first history))
            (get-actual-lines (rest history)))
      nil))

; get-actual-times - extracts the actual lines entries from
; the history list formatted by line-and-time-history.
; returns [rational]
(defun get-actual-times (history)
  (if (consp history)
      (cons (fourth (first history))
            (get-actual-times (rest history)))
      nil))

(defun create-pie-graph (data)
  (if (null data)
      "<i>No data</i>"
      (pie-graph data "")))

; get-replacements - creates an association list mapping replacement 
; keys onto the values that should be outputted.
;
; data :: [user-data] - entire history inputted
; returns association list
(defun get-replacements (data)
  (let* ((latest-project (first data))
         (attributes (user-data-attributes latest-project))
         (actual-estimate-history (line-and-time-history (rest data)))
         (loc-predictions (line-predictions 
                           (user-data-objects latest-project)))
         (added-prediction (if (second loc-predictions)
                               (second loc-predictions)
                               0))
         (modified-prediction (if (third loc-predictions)
                                  (third loc-predictions)
                                  0))
         (estimation-history 
          (get-estimated-lines actual-estimate-history))
         (size-projections 
          (if (>= (length data) 4) 
              (prediction-ranges estimation-history
                                 (get-actual-lines actual-estimate-history)
                                 (+ added-prediction modified-prediction))
              nil))
         (time-projections 
          (if (>= (length data) 4 ) 
              (prediction-ranges estimation-history
                                 (get-actual-times actual-estimate-history)
                                 (+ added-prediction modified-prediction))
              nil)))
    (list (cons "NAME" (attributes-name attributes))
          (cons "DATE" (format-date (attributes-date attributes)))
          (cons "INSTRUCTOR" 
                (prepare-field (attributes-instructor attributes)))
          (cons "PROGRAM" (attributes-program attributes))
          (cons "LANGUAGE" 
                (prepare-field (attributes-language attributes)))
          (cons "LOC-CERTAINTY-GRAPH" 
                (get-certainty-line-graph size-projections))
          (cons "TIME-CERTAINTY-GRAPH" 
                (get-certainty-line-graph time-projections))
          (cons "PREDICTED-LOC-BASE" 
                (prepare-field (first loc-predictions)))
          (cons "PREDICTED-LOC-ADDED" 
                (prepare-field (second loc-predictions)))
          (cons "PREDICTED-LOC-MODIFIED" 
                (prepare-field (third loc-predictions)))
          (cons "PREDICTED-LOC-REMOVED" 
                (prepare-field (fourth loc-predictions)))
          (cons "ACTUAL-LOC-ADDED" 
                (prepare-field (attributes-actual-lines attributes)))
          (cons "ACTUAL-LOC-BASE" 
                (prepare-field (attributes-actual-base attributes)))
          (cons "ACTUAL-LOC-MODIFIED" 
                (prepare-field (attributes-actual-modified attributes)))
          (cons "ACTUAL-LOC-REMOVED" 
                (prepare-field (attributes-actual-removed attributes)))
          ;(cons "ACTUAL-TIME" "TODO")
          ;(cons "MARGIN-INSIDE" "TODO")
          (cons "PROJECT-DEFECT-GRAPH" 
                (create-pie-graph (cumulative-defect-info 
                                   (list latest-project))))
          (cons "PROJECT-TIME-GRAPH" 
                (create-pie-graph (time-per-phase 
                                   (list latest-project))))
          (cons "CUMULATIVE-DEFECT-GRAPH" 
                (create-pie-graph (cumulative-defect-info data)))
          (cons "CUMULATIVE-TIME-GRAPH" 
                (create-pie-graph (time-per-phase data)))
          (cons "ACTUAL-ESTIMATED-GRAPH" 
                (scatter-plot actual-estimate-history 
                              "Actual-Estimate History"))
          (cons "ACTUAL-ESTIMATED-TABLE" 
                (actual-estimate-table actual-estimate-history))
          (cons "TIME-LOG-TABLE" (time-log-table latest-project))
          (cons "DEFECT-LOG-TABLE" (defect-log-table latest-project))
          (cons "PHASE-BAR-GRAPH" (time-by-day-graph latest-project)))))