(include-book "replace")

; sep is a string - e.g., "," "|"
; str is the string to be put in front of sep
; returns ",str" or "|str" or whatever
(defun insert-separator (sep str)
  (concatenate 'string str sep))

; inserts a separator into a list of strings
(defun insert-separator-list (sep xs)
  (if (endp (cdr xs)) (cons (car xs) nil)
      (cons (insert-separator sep (car xs)) 
            (insert-separator-list sep (cdr xs)))))

; takes a list of strings and returns 1 string
(defun string-list->string (xs)
  (if (endp (cdr xs)) (string-append (car xs) "")
      (string-append (car xs) (string-list->string (cdr xs)))))

; xss is a list of lists '( (VAL1) (VAL2) ... (VALN) )
; such that each sub-list has a string as the first element
; return all LABEL in a list
(defun fetch-labels (xss)
  (if (endp (cdr xss)) (cons (car (car xss)) nil)
      (cons (caar xss) (fetch-labels (cdr xss)))))

; xss is a list of conspairs '( (LABEL PERCENT) ... )
; return all PERCENT in a list as a string
(defun pie-graph-fetch-percents (xss)
  (if (endp (cdr xss)) (cons (rat->str (cadar xss) 0) nil)
      (cons (rat->str (cadar xss) 0) 
            (pie-graph-fetch-percents (cdr xss)))))

;xss is a list of lists '( (LABEL X Y) ... )
; return all X in a list as a string
(defun scatter-plot-fetch-x (xss)
  (if (endp (cdr xss)) (cons (rat->str (cadar xss) 0) nil)
      (cons (rat->str (cadar xss) 0)
            (scatter-plot-fetch-x (cdr xss)))))

;xss is a list of lists '( (LABEL X Y) ... )
; return all Y in a list as a string
(defun scatter-plot-fetch-y (xss)
  (if (endp (cdr xss)) (cons (rat->str (caddar xss) 0) nil)
      (cons (rat->str (caddar xss) 0)
            (scatter-plot-fetch-y (cdr xss)))))

; xs is a list of '(LABEL # # # ... )
; return all the values as a string
(defun line-graph-fetch-values (xs)
  (if (endp (cddr xs)) (cons (rat->str (cadr xs) 0) nil)
      (cons (rat->str (cadr xs) 0) (line-graph-fetch-values (cdr xs)))))

; xss is a list of list '( (LABEL # # # ... ) )
; returns all values as a string in a list of list
; '( ("#" "#" "#") ("#" ...) )
(defun bar-graph-fetch-values (xss)
  (if (endp (cdr xss)) (cons (line-graph-fetch-values (car xss)) nil)
      (cons (line-graph-fetch-values (car xss))
            (bar-graph-fetch-values (cdr xss)))))

; xss is a list of lists return from bar-graph-fetch-values
; turns each sublist into a string separated by commas
; returns a list of strings
(defun bar-graph-format-data (xss)
  (if (endp (cdr xss)) 
      (cons (string-list->string (insert-separator-list "," (car xss)))
            nil)
      (cons (string-list->string (insert-separator-list "," (car xss)))
            (bar-graph-format-data (cdr xss)))))

;http://chart.apis.google.com/chart?cht=p3&chd=t:60,
;   40&chs=250x100&chl=Hello|World
; xss is a list of conspairs '( (LABEL . PERCENT) ... )
; title is a string
; (instead of space, use + signs ... The+Title+of+Chart)
(defun pie-graph (xss title)
  (let* ((img "<img src='")
         (imgClose "'>")
         (baseURL "http://chart.apis.google.com/chart?cht=p3&chd=t:")
         (chartSizeLabel "&chs=300x150&chl=")
         (chartTitle (concatenate 'string "&chtt=" title))
         (labels (fetch-labels xss))
         (percents (pie-graph-fetch-percents xss))
         (labelString (string-list->string 
                       (insert-separator-list "|" labels)))
         (percentString (string-list->string 
                         (insert-separator-list "," percents))))
    (concatenate 'string 
                 img baseURL percentString 
                 chartSizeLabel labelString 
                 chartTitle imgClose)))

; xss is a list of lists '( (LABEL X Y) ... )
; title is a string
; (instead of space, use + signs ... The+Title+of+Graph
(defun scatter-plot (xss title)
  (let* ((img "<img src='")
         (imgClose "'>")
         (baseURL "http://chart.apis.google.com/chart?cht=s&chd=t:")
         (chartSize "&chs=300x200")
         (chartTitle (concatenate 'string "&chtt=" title))
         (chartAxesAndRange "&chxt=x,y,x,y&chxr=0,0,150,30|1,0,150,30")
         (chartLabelsAndPosition 
          "&chxl=2:|User-Estimated+LOC|3:|Actual+LOC&chxp=2,50|3,50")
         (chartMarkers "&chm=d,000000,1,1.0,10.0&chg=20,20")
         (labels (fetch-labels xss))
         (xvals (scatter-plot-fetch-x xss))
         (yvals (scatter-plot-fetch-y xss))
         (xString (string-list->string 
                   (insert-separator-list "," xvals)))
         (yString (string-list->string 
                   (insert-separator-list "," yvals))))
    (concatenate 'string
                 img baseURL xString "|" yString
                 chartAxesAndRange chartLabelsAndPosition
                 chartSize chartTitle chartMarkers imgClose)))

; Calculates the maximum of the given list
(defun maximum (xs)
  (if (consp (rest xs))
      (max (first xs)
           (maximum (rest xs)))
      (first xs)))

; Calculates the maximum value in any of the given lists
(defun max-data-value (xss)
  (if (consp (rest xss))
      (max (maximum (rest (first xss)))
           (max-data-value (rest xss)))
      (maximum (rest (first xss)))))

; xss is a list of lists '(("Label1" # # # #) ("Label2" # # #))
; xs is a list of x values '("10%" "20%" ... "90%")
; title is a title
(defun line-graph (xss xs title)
  (let* ((img "<img src='")
         (imgClose "'>")
         (baseURL "http://chart.apis.google.com/chart?cht=lc&chd=t:")
         (data1 (string-list->string 
                 (insert-separator-list "," (line-graph-fetch-values 
                                             (car xss)))))
         (data2 (string-list->string 
                 (insert-separator-list "," (line-graph-fetch-values 
                                             (cadr xss)))))
         (chartSize "&chs=450x300")
         (chartTitle (concatenate 'string "&chtt=" title))
         (lineColors "&chco=FF0000,00FF00")
         (axes "&chxt=x,y,x,y&chxr=1,0,150,30")
         (lineNames (concatenate 'string "&chdl=" 
                                 (string-list->string 
                                  (insert-separator-list "|" 
                                                (fetch-labels xss)))))
         (preCertainty "&chxl=0:|")
         (certainty (string-list->string (insert-separator-list "|" xs)))
         (postCertainty "|2:|Certainty|3:|LOC&chxp=2,50|3,50")
         (maxValue (rat->str (max-data-value xss) 0))
         (chartDim (concatenate 'string "&chds=0," maxValue))
         (dataScale (concatenate 'string "&chxr=1,0," maxValue)))
    (concatenate 'string
                 img baseURL data1 "|" data2
                 chartSize lineColors axes
                 lineNames preCertainty certainty postCertainty
                 chartTitle chartDim dataScale imgClose)))

(defun bar-graph (xss xs title)
  (let* ((img "<img src='")
         (imgClose "'>")
         (baseURL "http://chart.apis.google.com/chart?cht=bvs&chd=t:")
         (xaxis (string-list->string (insert-separator-list "|" xs)))
         (legend (string-list->string(insert-separator-list "|" 
                                        (fetch-labels xss))))
         (data (string-list->string 
                (insert-separator-list "|"
                                       (bar-graph-format-data
                                        (bar-graph-fetch-values xss)))))
         (colors "&chco=FFCC00,CC3300,CC6600,99CC00,CCCC00"))
    (concatenate 'string
                 img baseURL data colors
                 "&chdl=" legend
                 "&chxt=x,y&chxl=0:|" xaxis
                 "&chtt=" title "&chs=500x200"
       "&chbh=30,10,10&chds=0,240,0,240,0,240,0,240,0,240&chxr=1,0,240,60"
                 imgClose)))