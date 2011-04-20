(in-package "ACL2")

(include-book "output")

; clean-project-line - Strips comment and whitespace from a line in the 
; file. If the resulting string is empty, nil is returned. A comment is 
; anything following the optional pound sign.
;
; line :: string - line to clean
; returns string
(defun clean-project-line (line)
  (let* ((comment-stripped (take-to #\# (str->chrs line)))
         (trimmed (trim comment-stripped)))
    (if (endp trimmed)
        nil
        (chrs->str trimmed))))

; get-project-files - Extracts a list of project files from the given 
; lines. The project files are indicated by any line containing text 
; other than whitespace and comment. Each of them is appended with a 
; "txt" extension.
;
; lines :: [string]
; returns [string]
(defun get-project-files (lines)
  (if (endp lines) 
      nil
      (let ((parsed-line (clean-project-line (first lines))))
        (if (null parsed-line)
            (get-project-files (rest lines))
            (cons (string-append parsed-line ".txt")
                  (get-project-files (rest lines)))))))

; parse-inputs - Parses each project in the given list of projects. 
; The contents of each project should be contained in the file bundle
;
; project-files :: [string]
; input-fb :: file-bundle
;
; returns [user-data]
(defun parse-inputs (project-files input-fb)
  (if (endp project-files)
      nil
      (let ((first-parsed 
             (parse-file (get-from-bundle 
                          (first project-files) input-fb))))
        (if (stringp first-parsed)
            first-parsed
            (cons first-parsed
                  (parse-inputs (rest project-files) input-fb))))))

; Gets the list of projects to process, based on a "projects.txt" file;
;
; input-dir :: string - directory to find "projects.txt"
;
; returns string
(defun get-project-list (input-dir state)
  (mv-let (project-list-fb state)
          (read-bundle (list "projects.txt") input-dir state)
          (mv (if (stringp project-list-fb)
                  project-list-fb
                  (reverse (get-project-files 
                            (get-from-bundle "projects.txt" 
                                             project-list-fb))))
              state)))

; get-userdata :: gets the list of userdata from the given input
; directory
;
; returns [user-data]
(defun get-userdata (input-dir state)
  (mv-let (project-files state)
          (get-project-list input-dir state)
          (if (stringp project-files)
              (mv project-files state)
              (if (endp project-files)
                  (mv 
        "PSP++ cannot proceed because projects.txt contains no projects." 
                   state)
                  (mv-let (input-fb state)
                          (read-bundle project-files input-dir state)
                          (mv (if (stringp input-fb) 
                                  input-fb
                                  (parse-inputs project-files input-fb))
                              state))))))

; get-tempate :: reads a template file from the given directory. String 
; to be replaced in the template are indicated by |KEYS-OF-THIS-FORM|.
;
; returns [string]
(defun get-template (template-dir state)
  (mv-let (template-fb state)
          (read-bundle (list "template.html") template-dir state)
          (mv (if (stringp template-fb)
                  template-fb
                  (get-from-bundle "template.html" 
                                   template-fb))
              state)))

; Performs output calculations, applies the template, and writes the
; result to a file
;
; userdata :: [user-data] - entire history of projects
; template :: [string] - template to apply
; output-dir :: string - directory to write into
; state :: state
; returns (mv status state)
(defun do-output (userdatas template output-dir state)
  (let* ((replacements (get-replacements userdatas))
         (template-applied (template-replace template replacements))
         (program-name (attributes-program (user-data-attributes 
                                            (first userdatas))))
         (file-name (string-append program-name ".html"))) 
    (mv-let (error state)
            (write-bundle (add-to-bundle file-name
                                         template-applied
                                         (create-bundle))
                          output-dir
                          state)
            (mv (if (null error)
                    (string-append "Generated report for " program-name)
                    error)
                state))))


; Runs the PSP software with the given directories, generating a 
; report into the specified output directory.
;
; input-dir :: string - Input directory. This should contain the 
;  "projects.txt" file and all the project files.
; template-dir :: string - Directory containing template.html to use
; output-dir :: string : Direcotry to write to
; state :: state
(defun main (input-dir output-dir state)
  (mv-let (userdatas state)
          (get-userdata input-dir state)
          (if (stringp userdatas)
              (mv userdatas state)
              (mv-let (template state)
                      (get-template "template/" state)
                      (if (stringp template)
                          (mv template state)
                          (do-output userdatas template output-dir 
                                     state))))))