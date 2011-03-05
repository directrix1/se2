(module reader syntax/module-reader
  #:language modular-acl2-module-v
  #:read acl2-read
  #:read-syntax acl2-read-syntax
  #:wrapper1 read-all/module-begin
  #:whole-body-readers? #t

  (require "../../lang/acl2-module-v.ss"
           scheme/list scheme/match
           (prefix-in acl2- "../../lang/acl2-reader.ss"))

  (define (read-all/module-begin reader)
    (list (cons '#%module-begin (read-all reader))))

  (define (read-all reader)
    (let* ([term (reader)])
      (if (eof-object? term) null (cons term (read-all reader))))))
