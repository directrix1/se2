#|
Provides read and read-syntax so that we can use

  #reader(lib "acl2-reader.ss" "acl2" "private")(module . . .)
  
to write ACL2 modules.  (Teachpacks in particular use this.)
|#
(module acl2-reader mzscheme
  (require (file "acl2-readtable.ss"))
  (provide (rename acl2-read-syntax read-syntax)
           (rename acl2-read read))

  (define (make-acl2-reader reader)
    (lambda args
      (parameterize ([current-readtable acl2-readtable]
                     [read-square-bracket-as-paren #f]
                     [read-case-sensitive #f]
                     [read-accept-box #f]
                     [read-accept-reader #f]
                     [read-decimal-as-inexact #f])
        (apply reader args))))

  (define acl2-read-syntax
    (let* ([reader (make-acl2-reader read-syntax)])
      (case-lambda
        [() (reader)]
        [(name) (reader name)]
        [(name port) (reader name port)])))

  (define acl2-read
    (let* ([reader (make-acl2-reader read)])
      (case-lambda
        [() (reader)]
        [(port) (reader port)])))

  )
