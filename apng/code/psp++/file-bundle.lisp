(include-book "graph")

(set-state-ok t)

; read-single : Reads a single file at the given path and splits it 
; into a list of strings
; If an error occurs, it will return an mv(string, state). Otherwise, 
; it will return an (mv [string] state).
; 
; file-path :: string - path to read from
; state :: state
;
; returns (mv string state) or (mv [string] state)
(defun read-single (file-path state)
  (mv-let (input-str input-error state)
          (file->string file-path state)
          (mv (if input-error
                  (string-append "Error reading " file-path)
                  (split-on-whitespace input-str
                                       (list (code-char 10) 
                                             (code-char 13))))
              state)))

; create-bundle - Creates an empty bundle.
;
; returns bundle
(defun create-bundle ()
  nil)

; add-to-bundle - Adds to the file bundle a file with the given name 
; and contents
;
; file-name :: string - name of file to add, not including path
; file-contents :: [string] - lines of file
; bundle :: bundle to add to
;
; returns bundle
(defun add-to-bundle (file-name file-contents bundle)
  (cons (list file-name file-contents)
        bundle))

; get-from-bundle - Extracts a file of the given name from the bundle. 
; If the file is found, its lines are returned. Otherwise, nil is
; returned.
; 
; file-name :: string - name of file to extract, not including path
; bundle :: bundle - bundle to extract from
;
; returns nil or [string]
(defun get-from-bundle (file-name bundle)
  (if (endp bundle)
      nil
      (if (string-equal file-name (first (first bundle)))
          (second (first bundle))
          (get-from-bundle file-name (rest bundle)))))

; read-more - Internal helper function for read.
; file-bundle :: bundle - bundle to add to
; file-names :: [string] - file names to read, not including path
; directory :: string - directory to read each file from
; state :: state
;
; returns (mv bundle state) if successful, (mv bundle string otherwise)
(defun read-more (file-bundle file-names directory state)
  (if (endp file-names)
      (mv file-bundle state)
      (mv-let (contents state)
              (read-single (string-append directory (first file-names))
                           state)
              (if (stringp contents)
                  (mv contents state) ; error reading this file
                  (read-more (add-to-bundle (first file-names)
                                            contents
                                            file-bundle)
                             (rest file-names)
                             directory
                             state)))))

; read - Reads all the given files from the given directory. Returns an
; (mv bundle state) on success and an (mv string state) on failure.
;
; file-names :: [string] - file names to read, not including path
; directory :: string - directory to read each file from
; state :: state
;
; returns (mv bundle state) if successful, (mv bundle string otherwise)
(defun read-bundle (file-names directory state)
  (read-more (create-bundle) file-names directory state))

(defun take (n xs)
  (if (or (zp n) (endp xs))
      nil
      (cons (car xs) 
            (take (1- n) (rest xs)))))

; add-numbering - inserts a number into a file path. This allows 
; alternate files to be written if the file name requested is 
; already used.
; 
; If number is nil, the file path is not modified.
;
; file-path :: string
; number :: nat or nil
;
; returns string
(defun add-numbering (file-path number)
  (if (null number)
      file-path
      (let* ((chrs (str->chrs file-path)) 
             (after-dot (member-equal #\. chrs ))
             (before-dot (take (- (len chrs) (len after-dot)) chrs)))
        (string-concat (list (chrs->str before-dot)
                             " ("
                             (rat->str number 0)
                             ")"
                             (chrs->str after-dot))))))

; write-single - Writes a single file to the given path. 
(defun write-single (file-path file-contents counter state)
  (mv-let (error state)
          (string-list->file (add-numbering file-path counter) 
                             file-contents 
                             state)
          (if error
              (if (or (null counter) (< counter 20))
                  (write-single file-path 
                                file-contents
                                (if (null counter)
                                    2
                                    (1+ counter))
                                state)
                  (mv error state))
              (mv nil state))))

; write - Writes all files in a bundle to the given directory. If a 
; requested path is taken, it will append a number to the end of 
; the file.
;
; bundle :: bundle - bundle to write
; directory :: string - directory to write into
; state :: state
(defun write-bundle (bundle directory state)
  (if (endp bundle)
      (mv nil state)
      (mv-let (error state)
              (write-single (string-append directory (first 
                                                      (first bundle)))
                            (second (first bundle)) 
                            nil
                            state)
              (if (stringp error)
                  (mv error state)
                  (write-bundle (rest bundle) directory state)))))