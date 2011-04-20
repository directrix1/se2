#|
Extends drscheme:language:simple-settings with some ACL2 specific settings.
(file "acl2-language-unit.ss") imports these and uses them to implement
drscheme:language:module-based-language<%>.
|#
(module acl2-settings mzscheme
  (require (lib "struct.ss")
           (file "acl2-location-pref.ss")
           (file "admit-before-run-pref.ss"))
  (provide (struct acl2-settings (acl2-loc admit-before-run?))
           marshall-acl2-settings
           default-acl2-settings
           unmarshall-acl2-settings)
  
  (define-struct acl2-settings (acl2-loc admit-before-run?))
  
  (define acl2-settings->vector (make-->vector acl2-settings))
  
  (define marshall-acl2-settings acl2-settings->vector)
  
  (define default-acl2-settings 
    (make-acl2-settings 
     (path->string (get-acl2-location))
     (get-admit-before-run?)))
  
  (define unmarshall-acl2-settings
    (lambda (v)
      (make-acl2-settings (vector-ref v 0) (vector-ref v 1))))
  )