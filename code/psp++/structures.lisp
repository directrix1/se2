(defun make-code-object (name type base removed modified added comment)
  (list name type base removed modified added comment))

(defun code-object-name (object)
  (first object))

(defun code-object-type (object)
  (second object))

(defun code-object-base (object)
  (third object))

(defun code-object-removed (object)
  (fourth object))

(defun code-object-modified (object)
  (fifth object))

(defun code-object-added (object)
  (sixth object))

(defun code-object-comment (object)
  (seventh object))

(defun make-attributes (name program instructor date actual-lines 
                             actual-base actual-modified actual-removed 
                             language)
  (list name program instructor date actual-lines 
        actual-base actual-modified actual-removed language))

(defun attributes-name (attributes)
  (first attributes))

(defun attributes-program (attributes)
  (second attributes))

(defun attributes-instructor (attributes)
  (third attributes))

(defun attributes-date (attributes)
  (fourth attributes))

(defun attributes-actual-lines (attributes)
  (fifth attributes))

(defun attributes-actual-base (attributes)
  (sixth attributes))

(defun attributes-actual-modified (attributes)
  (seventh attributes))

(defun attributes-actual-removed (attributes)
  (eighth attributes))

(defun attributes-language (attributes)
  (ninth attributes))

(defun make-time-log (date phase start-time delta-time 
                           interruption-time comment)
  (list date phase start-time delta-time interruption-time comment))

(defun time-log-date (time-log)
  (first time-log))

(defun time-log-phase (time-log)
  (second time-log))

(defun time-log-start-time (time-log)
  (third time-log))

(defun time-log-delta-time (time-log)
  (fourth time-log))

(defun time-log-interruption-time (time-log)
  (fifth time-log))

(defun time-log-comment (time-log)
  (sixth time-log))

(defun make-defect (date type fix-time comment)
  (list date type fix-time comment))

(defun defect-date (defect)
  (first defect))

(defun defect-type (defect)
  (second defect))

(defun defect-fix-time (defect)
  (third defect))

(defun defect-comment (defect)
  (fourth defect))

(defun make-user-data (attributes objects defect-log time-log)
  (list attributes objects defect-log time-log))

(defun user-data-attributes (user-data)
  (first user-data))

(defun user-data-objects (user-data)
  (second user-data))

(defun user-data-defect-log (user-data)
  (third user-data))

(defun user-data-time-log (user-data)
  (fourth user-data))