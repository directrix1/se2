;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#| Team Steele
   Software Engineering I
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
  
  ;These functions just wrap the basiclex functions of the same name
  ;returning mv's instead of lists because they are a guaranteed size
  ;and I like using mv-let.
  (defun split-on-token-mv (tok xs)
    (let* ((res (split-on-token tok xs)))
      (mv (car res) (cadr res) (caddr res))))
  ;See above
  (defun span-mv (ps xs)
    (let* ((res (span ps xs)))
      (mv (car res) (cadr res))))
  ;See above
  (defun split-at-delimiter-mv (ds xs)
    (let* ((res (split-at-delimiter ds xs)))
      (mv (car res) (cadr res))))
  ;See above
  (defun splitoff-prefix-mv (ps xs)
    (let* ((res (splitoff-prefix ps xs)))
      (mv (car res) (cadr res) (caddr res))))

  ;xml-escape (unescapedchars) → returns string with bad chars replaced
  ;    with entities
  (defun xml-escape (unescapedchars)
    (if (null unescapedchars)
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
    (if (null attributes)
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
    (if (null xmlnodes)
        ""
        (let ((node (car xmlnodes))
              (rest (cdr xmlnodes)))
          (mv-let (nodename attributes children)
                  node
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
  ; returns (mv attributes remainingxmlstring)
  (defun xml-readnodeproperties (xmlchars)
    (mv-let (ws1 att1)
            (span-mv *whitespace* xmlchars)
            (if (null att1)
                (mv nil nil)
                (if (member (car att1) *endtagname*)
                    (mv nil att1)
                    (mv-let
                     (propname r1)
                     (split-at-delimiter-mv *endattrname* att1)
                     (mv-let
                      (r2 r3 propvstart)
                      (split-on-token-mv '(#\" ) r1)
                      (mv-let
                       (propvalue r4 rest)
                       (split-on-token-mv '(#\" ) propvstart)
                       (mv-let
                        (props rrest)
                        (xml-readnodeproperties rest)
                        (mv (cons (mv 
                                   (xml-unescape propname)
                                   (xml-unescape propvalue))
                                  props) rrest)))))
                    ))))
  
  ;xml-skipdontcares (xmlchars) → returns next xmlchars sans don't cares
  (defun xml-skipdontcares (xmlchars)
    (mv-let (ws1 tag1)
            (span-mv *whitespace* xmlchars)
            (if (null tag1)
                nil
                (if (equal (car tag1) #\< )
                    (if (equal (cadr tag1) #\? )
                        (mv-let (dontcare dctok therest)
                                (split-on-token-mv '(#\? #\> )
                                                   (cddr tag1))
                                (xml-skipdontcares therest))
                        (mv-let (matched dontcare comment)
                                (splitoff-prefix-mv '(#\! #\- #\- )
                                                    (cdr tag1))
                                (if (= (length matched) 3)
                                    (xml-skipdontcares
                                     (caddr 
                                      (split-on-token
                                       '(#\- #\- #\> ) comment)))
                                    tag1)))
                    xmlchars))))
  
  ;xml-readnodes (xmlchars) → returns (mv nodes remainingxmlstring)
  (defun xml-readnodes (xmlchars)
    (mv-let (ws content)
            (span-mv *whitespace* (xml-skipdontcares xmlchars))
            (if (null content)
                (mv nil nil)
                (if (equal (car content) #\< )
                    (mv-let (ws1 tag1)
                            (span-mv *whitespace* (cdr content))
                            (if (null tag1)
                                (mv nil nil)
                                (if (equal (car tag1) #\/ )
                                    (mv-let (dontcare dctok therest)
                                            (split-on-token-mv 
                                             '(#\> ) tag1)
                                            (mv nil therest))
                                    (mv-let (tagname tagattrs)
                                            (split-at-delimiter-mv
                                             *endtagname* tag1)
                                            (mv-let 
                                             (attribs tagend)
                                             (xml-readnodeproperties
                                              tagattrs)
                                             (mv-let
                                              (dc1 dc2 inner)
                                              (split-on-token-mv
                                               '(#\> ) tagend)
                                              (mv-let
                                               (nodes rest)
                                               (if (equal
                                                    (car tagend)
                                                    #\/ )
                                                   (mv nil inner)
                                                   (xml-readnodes
                                                    inner))
                                               (mv-let
                                                (morenodes morerest)
                                                (xml-readnodes rest)
                                                (mv
                                                 (cons 
                                                  (mv
                                                   (xml-unescape
                                                    tagname)
                                                   attribs
                                                   nodes) morenodes)
                                                 morerest)))))))))
                    (mv-let (thetext therest)
                            (split-at-delimiter-mv '(#\< ) content)
                            (if (null thetext)
                                (xml-readnodes therest)
                                (mv-let (nodelist restoftext)
                                        (xml-readnodes therest)
                                        (mv
                                         (cons
                                          (mv 'text nil
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
