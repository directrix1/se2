;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")

#| Team Steele
   Software Engineering II
   MminidomSerializer

   Software that creates a document object model from XML input, or
   creates XML from DOM input.
|#
(in-package "ACL2")

(require "../interfaces/IminidomSerializer.lisp")
(require "../interfaces/Ibasiclex.lisp")

;(defconst *whitespace* '(#\Space #\Return #\Newline #\Linefeed #\Tab ))
;(defconst *endtagname* (cons #\> (cons #\/ *whitespace*)))
;(defconst *endattrname* (cons #\= *whitespace*))

(module MminidomSerializer
  (import Ibasiclex)

  (include-book "list-utilities" :dir :teachpacks)
  
  ;xml-escape (unescapedchars) → returns string with bad chars replaced
  ;    with entities
  (defun xml-escape (unescapedchars)
    (if (endp unescapedchars)
        ""
        (let ((char (car unescapedchars))
              (rest (cdr unescapedchars)))
          (string-append
           (cond
            ((equal char #\&) "&amp;")
            ((equal char #\<) "&lt;")
            ((equal char #\>) "&gt;")
            ((equal char #\') "&apos;")
            ((equal char #\") "&quot;")
            (t (chrs->str (list char))))
           (xml-escape rest)
           ))))
  
  ;xml-serialize-attributes (attributes) → Returns a string that is
  ;    xml that represents the passed in attribute list.
  (defun xml-serialize-attributes (attributes)
    (if (endp attributes)
        ""
        (let ((attribute (car attributes))
              (rest (cdr attributes)))
          (concatenate
           'string
           " "
           (xml-escape (str->chrs (car attribute)))
           "=\""
           (xml-escape (str->chrs (cadr attribute)))
           "\""
           (xml-serialize-attributes rest)))))
           
  
  ;xml-serizlize-nodes (xmlnodes) → Returns a string containing xml
  ;    nodes that represents the node list, xmlnodes.
  (defun xml-serialize-nodes (xmlnodes)
    (if (endp xmlnodes)
        ""
        (let ((node (car xmlnodes))
              (rest (cdr xmlnodes)))
          (let  ((nodename (car node))
                 (attributes (cadr node))
                 (children (caddr node)))
                  (string-append
                   (if (equal nodename 'text)
                       (xml-escape (str->chrs children))
                       (concatenate 'string
                        "<"
                        (xml-escape (str->chrs nodename))
                        (xml-serialize-attributes attributes)
                        (if (null children)
                            "/>"
                            (concatenate 'string
                             ">"
                             (xml-serialize-nodes children)
                             "</"
                             (xml-escape (str->chrs nodename))
                             ">"))))
                   (xml-serialize-nodes rest))))))

  ;xml-serizlize-dom (xmlnode) → Returns a string containing an xml
  ;    document that represents the dom passed in through xmlnode.
  (defun xml-serialize-dom (xmlnode)
        (string-append
         "<?xml version=\"1.0\"?>"
         (xml-serialize-nodes (list xmlnode)))
    )

  ;xml-unescape (escapedchars) → string with entities replaced
  (defun xml-unescape (escapedchars)
    (if (and 
         (consp escapedchars)
         (standard-char-listp escapedchars))
        (let* ((sot (split-on-token '(#\& ) escapedchars))
               (pre (car sot))
               (post (caddr sot))
               (sot1 (split-on-token '(#\; ) post))
               (thechar (car sot1))
               (theend (caddr sot1))
               (thecharstr (chrs->str thechar)))
              (concatenate 'string
               (chrs->str pre)
                (if (string-equal "amp" thecharstr)
                    "&"
                    (if (string-equal "lt" thecharstr)
                        "<"
                        (if (string-equal "gt" thecharstr)
                            ">"
                            (if (string-equal "quot" thecharstr)
                                "\""
                                (if (string-equal "apos" thecharstr)
                                    "'"
                                    "")))))
                (xml-unescape theend)))
        ""))
  
  ;xml-readnodeproperties (xmlchars) →
  ; returns (list attributes remainingxmlstring)
  (defun xml-readnodeproperties (xmlchars)
    (let* ((t1 (span *whitespace* xmlchars))
           (ws1 (car t1))
           (att1 (cadr t1)))
            (if (null att1)
                (list nil nil)
                (if (member (car att1) *endtagname*)
                    (list nil att1)
                    (let* (
                     (t2 (split-at-delimiter *endattrname* att1))
                     (propname (car t2))
                     (r1 (cadr t2)))
                     (let* (
                      (t3 (split-on-token '(#\" ) r1))
                      (r2 (car t3))
                      (r3 (cadr t3))
                      (propvstart (caddr t3)))
                      (let* (
                       (t4 (split-on-token '(#\" ) propvstart))
                       (propvalue (car t4))
                       (r4 (cadr t4))
                       (rest (caddr t4))
                       )
                       (let* (
                        (t5 (xml-readnodeproperties rest))
                        (props (car t5))
                        (rrest (cadr t5))
                        )
                        (list (cons (list 
                                   (xml-unescape propname)
                                   (xml-unescape propvalue))
                                  props) rrest)))))
                    ))))
  
  ;xml-skipdontcares (xmlchars) → returns next xmlchars sans don't cares
  (defun xml-skipdontcares (xmlchars)
    (let* (
           (t1 (span *whitespace* xmlchars))
           (ws1 (car t1))
           (tag1 (cadr t1))
           )
            (if (null tag1)
                nil
                (if (equal (car tag1) #\< )
                    (if (equal (cadr tag1) #\? )
                        (let* (
                                 (t2 (split-on-token '(#\? #\> )
                                                   (cddr tag1)))
                                 (therest (caddr t2))
                                 )
                                (xml-skipdontcares therest))
                        (let* (
                               (t2 (splitoff-prefix '(#\! #\- #\- )
                                                    (cdr tag1)))
                               (matched (car t2))
                               (comment (caddr t2))
                               )
                                (if (= (length matched) 3)
                                    (xml-skipdontcares
                                     (caddr 
                                      (split-on-token
                                       '(#\- #\- #\> ) comment)))
                                    tag1)))
                    xmlchars))))
  
  ;xml-readnodes (xmlchars) → returns (list nodes remainingxmlstring)
  (defun xml-readnodes (xmlchars)
    (let* (
           (t1 (span *whitespace* (xml-skipdontcares xmlchars)))
           (ws (car t1))
           (content (cadr t1))
           )
            (if (null content)
                (list nil nil)
                (if (equal (car content) #\< )
                    (let* (
                           (t2 (span *whitespace* (cdr content)))
                           (ws1 (car t2))
                           (tag1 (cadr t2))
                           )
                            (if (null tag1)
                                (list nil nil)
                                (if (equal (car tag1) #\/ )
                                    (let* (
                                             (t3 (split-on-token 
                                              '(#\> ) tag1))
                                             (therest (caddr t3))
                                             )
                                            (list nil therest))
                                    (let* (
                                           (t4 (split-at-delimiter
                                             *endtagname* tag1))
                                           (tagname (car t4))
                                           (tagattrs (cadr t4))
                                           )
                                            (let* ( 
                                             (t5 (xml-readnodeproperties
                                              tagattrs))
                                             (attribs (car t5))
                                             (tagend (cadr t5))
                                             )
                                             (let* (
                                              (t6 (split-on-token
                                               '(#\> ) tagend))
                                              (inner (caddr t6))
                                              )
                                              (let* (
                                               (t7 (if (equal
                                                    (car tagend)
                                                    #\/ )
                                                   (list nil inner)
                                                   (xml-readnodes
                                                    inner)))
                                               (nodes (car t7))
                                               (rest (cadr t7))
                                               )
                                               (let* (
                                                (t8 (xml-readnodes rest))
                                                (morenodes (car t8))
                                                (morerest (cadr t8))
                                                )
                                                (list
                                                 (cons 
                                                  (list
                                                   (xml-unescape
                                                    tagname)
                                                   attribs
                                                   nodes) morenodes)
                                                 morerest)))))))))
                    (let* (
                           (t9 (split-at-delimiter '(#\< ) content))
                           (thetext (car t9))
                           (therest (cadr t9))
                           )
                            (if (null thetext)
                                (xml-readnodes therest)
                                (let* (
                                       (t10 (xml-readnodes therest))
                                       (nodelist (car t10))
                                       (restoftext (cadr t10))
                                       )
                                        (list
                                         (cons
                                          (list 'text nil
                                              (xml-unescape
                                               (append ws thetext)))
                                          nodelist)
                                         restoftext))))))))
  
  ;xml-readnode (xmlchars) → returns the root node from xmlstring
  (defun xml-readnode (xmlchars)
    (let ((result (xml-readnodes (str->chrs xmlchars))))
      (if (null (car result))
          nil
          (caar result))))
  
  (export IminidomSerializer))
