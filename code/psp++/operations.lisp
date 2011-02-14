(in-package "ACL2")

(include-book "structures")
(include-book "date-time")

; add-phases - creates a list of all phases that occur in the time log.
;
; time-logs :: [time-log] - time logs to search through
; phases :: [string] - running list of phases that have occurred.
; returns [string]
(defun add-phases (time-logs phases)
  (if (consp time-logs)
      (let* ((first-entry (first time-logs))
             (first-phase (time-log-phase first-entry)))
        (add-phases (rest time-logs)
                    (if (member-equal first-phase phases)
                        phases
                        (cons first-phase phases))))
      phases))

; sum-delta-times - calculates the total time spent in the given phase,
; divided into days.
;
; time-logs - time logs to search through
; phase :: string - phase to search for
; totals :: [(natural . rational)]
;
; returns an association list of dates (naturals) onto rationals
(defun sum-delta-times (time-logs phase totals)
  (if (consp time-logs)
      (sum-delta-times 
       (rest time-logs)
       phase
       (if (string-equal phase
                         (time-log-phase (first time-logs)))
           (let* ((entry (first time-logs))
                  (day (time-log-date entry))
                  (existing-entry (assoc-equal day totals))
                  (existing-time (if (null existing-entry)
                                     0
                                     (cdr existing-entry)))
                  (interruption-time (time-log-interruption-time entry))
                  (new-time (+ (time-log-delta-time entry)
                               (- (if (null interruption-time) 
                                      0 interruption-time))
                               existing-time)))
             (cons (cons day new-time)
                   totals))
           totals))
      totals))

; times-in-range - generates a list times for the dates in the 
; given range
;
; sum-alist :: [(natural, rational)] - association list of naturals 
; onto times spent
; low-date :: nat - lower bound of dates to check, in days since 1980
; high-day :: nat - upper bound of dates to check, in days since 1980
;
; returns [rational]
(defun times-in-range (sum-alist low-date high-date)
  (declare (xargs :measure (acl2-count (+ 1 (- low-date) high-date ))))
  (if (and (integerp low-date) (integerp high-date) 
           (<= low-date high-date))
      (let* ((entry (assoc-equal low-date sum-alist))
             (value (if (null entry) 
                        0
                        (cdr entry))))
        (cons value (times-in-range sum-alist (1+ low-date) high-date)))
      nil))

; phase-time-by-day - generates a list of lists, where each list is 
; of the form (phase-name day1 day2 ... dayn).
(defun phase-time-by-day (phase-names time-logs min-date max-date)
  (if (consp phase-names)
      (cons (cons (first phase-names)
                  (times-in-range (sum-delta-times time-logs
                                                   (first phase-names)
                                                   nil)
                                  min-date
                                  max-date))
            (phase-time-by-day (rest phase-names) time-logs min-date 
                               max-date))
      nil))

; mix-max-day - finds the minimum and maximum date from those listed 
; in the time log.
(defun min-max-day (time-logs min-date max-date)
  (if (consp time-logs)
      (let* ((first-log (first time-logs))
             (first-date (time-log-date first-log)))
        (min-max-day (rest time-logs)
                     (if (null min-date)
                         first-date 
                         (min min-date first-date))
                     (if (null max-date)
                         first-date
                         (max max-date first-date))))
      (cons min-date max-date)))

; Formats all the dates between 'from' and 'to' as <day> <month> strings
; from :: nat - days since 1980 to start from
; to :: nat - days since 1980 to end on
; returns [string]
(defun format-date-range (from to)
  (if (and (integerp from) (integerp to) (<= from to))
      (cons (format-month-and-day from)
            (format-date-range (1+ from) to))
      nil))

; time-by-day :: Returns information about the time spent in each phase
; on each day.
;
; Return value is of the form
;  (((phaseA timeA1 timeA2 ... timeAN)
;    ...
;    (phaseZ timeZ1 timeZ2 ... timeZN))
;   (date-string1 date-string2 ... date-stringN))
(defun time-by-day (userdata)
  (if (endp (user-data-time-log userdata))
      (list nil nil)
      (let* ((time-logs (user-data-time-log userdata))
             (phase-names (add-phases time-logs nil))
             (min-max (min-max-day time-logs nil nil)))
        
        (list (phase-time-by-day phase-names 
                                 time-logs 
                                 (car min-max)
                                 (cdr min-max))
              (format-date-range (car min-max)
                                 (cdr min-max))))))

; add-project-defect-types - adds all types of defects that occur in 
; the given list of defects to the list
;
; returns [string]
(defun add-project-defect-types (defects types)
  (if (consp defects)
      (let* ((first-defect (first defects))
             (first-type (defect-type (first defects))))
        (add-phases (rest defects)
                    (if (member-equal first-type types)
                        types
                        (cons first-type types))))
      types))

; add-defect-types - adds all types of defects that occur in the given
; list of projects to the list
;
; returns [string]
(defun add-defect-types (projects types)
  (if (consp projects)
      (add-defect-types (rest projects)
                        (add-project-defect-types 
                         (user-data-defect-log (first projects))
                         types))
      types))

; project-defect-time-of-type - sums the fix time of all defects of the 
; given type in the list of defects
;
; returns rational
(defun project-defect-time-of-type (defects type)
  (if (consp defects)
      (+ (if (string-equal type (defect-type (first defects)))
             (defect-fix-time (first defects))
             0)
         (project-defect-time-of-type (rest defects) type))
      0))

; project-defect-time-of-type - sums the fix time of all defects of 
; the given type in the list of projects
;
; returns rational
(defun defect-time-of-type (projects type)
  (if (consp projects)
      (+ (project-defect-time-of-type (user-data-defect-log 
                                       (first projects)) type)
         (defect-time-of-type (rest projects) type))
      0))

; project-defect-time-of-type - sums the fix time of all defects of the 
; given types in the list of projects. The return value is a list of 
; list, each containing the defect type followed by the total time. 
;
; returns ((string [rational]))
(defun defect-time-of-types (projects types)
  (if (consp types)
      (cons (list (first types)
                  (defect-time-of-type projects (first types)))
            (defect-time-of-types projects (rest types)))
      nil))

; cumulative-defect-info - sums the time spent on each type of defect 
; in the given list of projects. The return value is a list of list, 
; each containing the defect type followed by the total time. 
;
; returns ((string [rational]))
(defun cumulative-defect-info (projects)
  (defect-time-of-types projects (add-defect-types projects nil)))

; line-predictions - sums the predictions for the various types of
; lines: base, added, modified, and removed, in that order.
;
; code-objects :: [code-object] - data to sum over
;
; returns (rational rational rational rational)
(defun line-predictions (code-objects)
  (if (consp code-objects)
      (let* ((code-object (first code-objects))
             (base (code-object-base code-object))
             (added (code-object-added code-object))
             (modified (code-object-modified code-object))
             (removed (code-object-removed code-object))
             (rest-predictions (line-predictions (rest code-objects))))
        (list (+ (if base base 0)
                 (first rest-predictions))
              (+ (if added added 0)
                 (second rest-predictions))
              (+ (if modified modified 0)
                 (third rest-predictions))
              (+ (if removed removed 0)
                 (fourth rest-predictions))))
      (list 0 0 0 0)))

; total-time - Calculates the total time spent in the given list of 
; time logs entries
;
; returns rational
(defun total-time (time-logs)
  (if (consp time-logs)
      (+ (let ((delta (time-log-delta-time (first time-logs)))
               (int (time-log-interruption-time (first time-logs))))
           (- delta (if int int 0)))
         (total-time (rest time-logs)))
      0))

; line-and-time-history - Generates data used for making PSP-type 
; predictions.
; The return value contains one entry per project, and each is of the
; form (project-name predicted-lines actual-lines actual-time)
(defun line-and-time-history (projects)
  (if (consp projects)
      (let* ((project (first projects))
             (attributes (user-data-attributes project))
             (actual-added (attributes-actual-lines attributes))
             (actual-modified (attributes-actual-modified attributes)))
        (if (or actual-modified actual-added)
            (cons (list (attributes-program attributes)
                        (second (line-predictions 
                                 (user-data-objects project)))
                        (+ (if actual-added actual-added 0)
                           (if actual-modified actual-modified 0))
                        (total-time (user-data-time-log project)))
                  (line-and-time-history (rest projects)))
            (line-and-time-history (rest projects))))
      nil))

; Generates a list of lists, each one holding a phase name
; and the time spent in that phase.
(defun project-time-by-phase (time-logs phase)
  (if (consp time-logs)
      (+ (let* ((time-log (first time-logs))
                (delta (time-log-delta-time time-log))
                (interruption (time-log-interruption-time time-log))
                (this-phase (time-log-phase time-log)))
           (if (string-equal phase this-phase)
               (- delta
                  (if interruption interruption 0))
               0))
         (project-time-by-phase (rest time-logs) phase))
      0))

; Calculates the time spent in this phase
(defun time-by-phase (projects phase)
  (if (consp projects)
      (+ (project-time-by-phase (user-data-time-log (first projects)) 
                                phase)
         (time-by-phase (rest projects) phase))
      0))

; Calculates the time spent in these phases
(defun time-by-phases (projects phases)
  (if (consp phases)
      (cons (list (first phases)  
                  (time-by-phase projects (first phases)))
            (time-by-phases projects (rest phases)))
      nil))

; Adds phases for all projects to the list
(defun add-project-phases (data phases)
  (if (consp data)
      (add-project-phases (rest data)
                          (add-phases (user-data-time-log (first data))
                                      phases))
      phases))

; Generates information about the cumulative time spent in each
; phase. Returns pairs of phase-name phase-time.
; date :: [user-data]
; returns [(string rational)]
(defun time-per-phase (data)
  (time-by-phases data
                  (add-project-phases data nil)))