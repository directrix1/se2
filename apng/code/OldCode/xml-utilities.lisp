(in-package "ACL2")
(include-book "list-utilities" :dir :teachpacks)
(include-book "char-utilities")
(include-book "basiclex")
(include-book "basiclex-ver")

(defconst *q* (list #\")) ; quote-mark for building strings
(defconst *=* (list #\=)) ; equals-sign for building attributes
(defconst *<* (list #\<)) ; opening angle-bracket for building tags
(defconst *>* (list #\>)) ; closing angle-bracket for building tags
(defconst */* (list #\/)) ; slash, for building close-tag markers)

; tag-name = name of tag, sans angle brackets, not ""  (string)
; chrs = chrs in well-formed sequence of xml tags      (list of chars)
; (tag-content+ tag-name chrs) = (attrs content after-tag)
;   where attrs     = chars between <tag-name and >    (list of chars)
;         content   = characters enclosed by the tag   (list of chars)
;         after-tag = characters after the closing tag (list of chars)
(defun tag-content-given-open/close (tagopn tagcls chrs)
  (let* ((3way      (split-on-token tagopn chrs)) ;look for tagopn
         (present?  (cadr 3way))             ; found if 2nd is non-nil
         (after-opn (caddr 3way)))           ; 3rd =stuff after tagopn
    (if present?
        (let* ((atts+     (split-at-delimiter *>* after-opn))
               (attr      (car atts+))       ; 1st = attribute part
               (>+        (cadr atts+))      ; 2nd starts at closing >
               (content+  (split-on-token tagcls >+))
               (content   (cdar content+))   ; 1st=content (skip >)
               (after-cls (caddr content+))) ; 3rd=stuff after tagcls
          (list attr content after-cls))
        nil)))                               ; tag not present
(defun tag-content+ (tag-name chrs)
  (let* ((tagopen (string-append "<"  tag-name))
         (tagclos (string-append "</" (string-append tag-name ">"))))
    (tag-content-given-open/close tagopen tagclos chrs)))

; attr-name = attribute name, not ""                   (string)
; chrs = attr part of tag, ie chars between <tag and > (list of chars)
; (attribute-value attr chrs) = chars between quotes   (list of chars)
;                             = 0 if attr not in chrs  (natural numbr)
;   Note: nil cannot signal "not present", attr value could be empty
(defun attribute-value (attr-name chrs)
  (let* ((3way (split-on-token attr-name chrs))
         (present? (cadr 3way))) ; 2nd non-nil iff attr not present
    (if present?
        (let* ((afnam (caddr 3way))
               (at=   (cadr (span *whitespace* afnam))) ; =...
               (af=   (cdr at=))                        ; after =
               (atq   (cadr (span *whitespace* af=)))   ; "...
               (afq   (cdr atq))                        ; after "
               (afq+  (split-at-delimiter *q* afq))     ; to next "
               (attr  (car afq+))                       ; "...attr..."
               (atq2  (cadr afq+)))                     ; after "..."
          (if (and (equal (car at=)  #\=)  ; attr-name= ...
                   (equal (car atq)  #\")  ; attr-name=" ...
                   (equal (car atq2) #\")) ; attr-name="..."
              attr
              0)) ; = or quotes missing
        0)))      ; attribute name missing

; chrs = suffix of xml file (list of characters)
; tag-name = string
; (tag-packets = (a1 c1 a2 c2 ...)
;  where a1 = attribute part of 1st occurrence of tag (list of chars)
;        c1 = contents part of 1st occurrence of tag (list of chars)
;        a2, c2 ... attributes and contents from remaining occurrences
(defun tag-packets (tag-name chrs)
  (declare
   (xargs :measure (len chrs)
          :hints (("Goal"
                   :use ((:instance splitoff-delivers-shorter-list
                                    (ps (str->chrs tag-name))
                                    (xs chrs)))))))
  (let* ((tc+ (tag-content+ tag-name chrs))
         (a   (car tc+))    ; attribute part of first tag
         (c   (cadr tc+))   ; contents part of first tag
         (aft (caddr tc+))) ; stuff beyond first tag
    (if (null tc+)
        nil
        (cons a (cons c (tag-packets tag-name aft))))))
