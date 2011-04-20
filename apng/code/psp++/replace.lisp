(include-book "utilities")

; chrs [characters] :: list of characters to be scanned
; so-far string :: string seen so far
; div character :: character to split at
; 
; returns a list [string (characters)]
; split at div, return a list where string is the chrs->str of the 
; character seen before div and (characters) is the remaining 
; characters in [characters] after div
(defun split2 (chrs so-far div)
  (if (consp chrs)
      (if (equal (first chrs) div)
          (list (reverse (chrs->str so-far)) (rest chrs))
          (split2 (rest chrs) 
                  (cons (first chrs) so-far)
                  div))
      (list (reverse (chrs->str so-far)) nil)))

; chrs [characters] :: list of characters
;
; returns t or nil
; if chrs contains only aA - zZ or a hyphen, then return true
; else return false
(defun possible-key (chrs)
  (if (consp chrs)
      (let ((first-char-code (char-code (first chrs))))
        (and (or (and (>= first-char-code 65) ; At least A
                      (<= first-char-code 90)); At most Z
                 (= first-char-code 45)) ;Hyphen
             (possible-key (rest chrs))))
      t))

; chunks [string] :: list of strings
; replacements [(string string)] :: list of conspairs of strings
;
; if the string in chunks is found in replacements, replace it with
;   another string
; (chunks-replace (list "NO" "FUN") '(("NO" . "GO") ("FUN" . "AWAY"))) 
;    -> (list "GO" "AWAY")
(defun chunks-replace (chunks replacements)
  (if (consp chunks)
      (let ((lookup (if (possible-key (str->chrs (first chunks)))
                        (assoc-string-equal (first chunks)
                                            replacements)
                        nil)))
        (cons (if (consp lookup)
                  (cdr lookup)
                  (first chunks))
              (chunks-replace (rest chunks) replacements)))
      nil))

; chrs [characters] :: list of characters
;
; returns [string]
; splits a list of characters at the | symbol
; returns a list consisting of strings, where each string is the
; characters appearing before a |
(defun bar-split (chrs)
  (if (endp chrs)
      nil
      (let ((first-division (split2 chrs nil #\|)))
        (cons (first first-division)
              (bar-split (second first-division))))))

; line :: string
; replacements [(string string)] :: list of conspairs of strings
;
; if line contains a string surrounded by pipes |EXAMPLE|,
; this method looks and sees if |EXAMPLE| exists in the replacements
; list, and if it is, |EXAMPLE| is replaced with some other string
(defun line-replace (line replacements)
  (string-concat 
   (chunks-replace (bar-split (str->chrs line)) replacements)))

; template-replace - Performs a replacement on a template list 
;  of strings.
; Any substring of the form |KEY| in the template is replaced by the
;  cooresponding value. Keys are mapped to values by an association list.
; Keys must be made up of only capital A-Z and the dash.
;
; template [string] :: list of list of strings
; replacements [(string . string)] :: list of key-value conspairs 
; of strings
;
; returns [string]
(defun template-replace (template replacements)
  (if (endp template)
      nil
      (cons (line-replace (first template)
                          replacements)
            (template-replace (rest template)
                              replacements))))