#lang scheme

(require syntax/moddep planet/util
         (for-template scheme/base))

(provide acl2-module-v
         modular-acl2-module-v
         teachpack-path
         make-teachpack-require-syntax
         make-dracula-spec)

(define dracula-package (this-package-version))
(define dracula-package/no-version
  (list (this-package-version-owner)
        (this-package-version-name)))

(define (make-dracula-spec #:version? [version? #t] file . dirs)
  `(planet ,file
           ,(if version?
                dracula-package
                dracula-package/no-version)
           ,@dirs))

(define (make-teachpack-require-syntax file)
  (list #'planet file dracula-package "teachpacks"))

(define teachpack-v (list 'planet "teachpacks" dracula-package))

(define backslash-pattern #rx"\\\\")

(define (backslashes->forward-slashes str)
  (regexp-replace* backslash-pattern str "/"))

(define teachpack-path
  (string-append (backslashes->forward-slashes
                  (path->string 
                   (resolve-module-path teachpack-v #f)))
                 "/"))

(define acl2-module-v (make-dracula-spec "dracula.ss" "lang"))

(define modular-acl2-module-v (make-dracula-spec "main.ss" "modular"))
