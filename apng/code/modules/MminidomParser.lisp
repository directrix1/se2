;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")

#| Team Steele
   Software Engineering II
   MminidomParser

   Software that parses a minidom tree.
|#

(in-package "ACL2")

(require "../interfaces/IminidomParser.lisp")
(require "../interfaces/Ibasiclex.lisp")

;(defconst *whitespace* '(#\Space #\Return #\Newline #\Linefeed #\Tab ))
;(defconst *endtagname* (cons #\> (cons #\/ *whitespace*)))
;(defconst *endattrname* (cons #\= *whitespace*))

(module MminidomParser
  (import Ibasiclex)

  (include-book "list-utilities" :dir :teachpacks)

  ;xml-text (text) → returns a text node with specified text.
  (defun xml-text (text)
    (list 'text nil text))

  ;xml-node (nodetype attributes children) → returns a node with specified
  ;  nodetype attributes and children.
  (defun xml-node (nodetype attributes children)
    (list nodetype attributes children))

  ;xml-attribute (attrname attrvalue) → returns an attribute with specified
  ;  name and value.
  (defun xml-attribute (attrname attrvalue)
    (list attrname attrvalue))
  
  ;xml-getnodetype (node) → return the type of the node
  (defun xml-getnodetype (node)
    (car node))
  
  ;xml-getattrlist (node) → return the list of attributes of the node
  (defun xml-getattrlist (node)
    (cadr node))
  
  ;xml-getchildren (node) → return the children of the nodes
  (defun xml-getchildren (node)
    (caddr node))
  
  ;xml-getattrname (attribute) → return the name of the attribute
  (defun xml-getattrname (attribute)
    (car attribute))
  
  ;xml-getattrvalue (attribute) → return the value of the attribute
  (defun xml-getattrvalue (attribute)
    (cadr attribute))
  
  ;xml-getnodes (node nodename) → returns children of node with type
  ;  nodename
  (defun xml-getnodes (node nodename)
    (if (= (length (caddr node)) 0)
        nil
        (let ((curnode (car (caddr node)))
              (rest (cdr (caddr node))))
          (if (equal (car curnode) 'text)
              (xml-getnodes (list nil nil rest) nodename)
              (if (string-equal (car curnode) nodename)
                  (cons curnode (xml-getnodes (list nil nil rest) nodename))
                  (xml-getnodes (list nil nil rest) nodename))))))
  
  ;xml-getdeepnodes (node nodename) → returns children of node with type
  ;  nodename searching recursively using DFS with node as root.
  (defun xml-getdeepnodes (node nodename)
    (if (= (length (caddr node)) 0)
        nil
        (let ((curnode (car (caddr node)))
              (rest (cdr (caddr node))))
          (if (equal (car curnode) 'text)
              (xml-getdeepnodes (list nil nil rest) nodename)
              (if (string-equal (car curnode) nodename)
                  (cons curnode 
                        (xml-getdeepnodes 
                         (list 
                          nil 
                          nil
                          (concatenate 'list (caddr curnode) rest))
                         nodename))
                  (xml-getdeepnodes 
                   (list
                    nil
                    nil 
                    (concatenate 'list (caddr curnode) rest))
                   nodename))))))
  
  ;xml-getnode (node nodename) → returns first child node with type
  ;  nodename
  (defun xml-getnode (node nodename)
    (car (xml-getnodes node nodename)))
  
  ;xml-getdeepnode (node nodename) → returns first child node with type
  ;  nodename searching recursively using DFS with node as root.
  (defun xml-getdeepnode (node nodename)
    (if (= (length (caddr node)) 0)
        nil
        (let ((curnode (car (caddr node)))
              (rest (cdr (caddr node))))
          (if (equal (car curnode) 'text)
              (xml-getdeepnode (list nil nil rest) nodename)
              (if (string-equal (car curnode) nodename)
                  curnode
                  (xml-getdeepnode 
                   (list
                    nil
                    nil
                    (concatenate 'list (caddr curnode) rest))
                   nodename))))))
  
  ;xml-getattribute (node attributename) → returns the value of node's
  ; attribute with name attributename
  (defun xml-getattribute (node attributename)
    (if (= (length (cadr node)) 0)
        ""
        (let ((curattr (car (cadr node)))
              (rest (cdr (cadr node))))
          (if (string-equal (car curattr) attributename)
              (cadr curattr)
              (xml-getattribute (list nil rest nil) attributename)))))
  
  
  ;xml-gettext (node) → returns the composite of all text inside of a node
  (defun xml-gettext (node)
    (if (null node)
        ""
        (if (= (length (caddr node)) 0)
            ""
            (let ((curnode (car (caddr node)))
                  (rest (cdr (caddr node))))
              (if (equal (car curnode) 'text)
                  (string-append (caddr curnode)
                                 (xml-gettext (list nil nil rest)))
                  (string-append (xml-gettext curnode)
                                 (xml-gettext (list nil nil rest))))))))
  
  ;xml-isattribute (attribute) → returns true iff attribute is an list of
  ;  length 2 with both elements of the list being strings
  (defun xml-isattribute (attribute)
    (and
     (true-listp attribute)
     (equal (length attribute) 2)
     (stringp (car attribute))
     (stringp (cadr attribute))))
  
  ;xml-isattributelist (attributes) → returns true iff attributes
  ;  is nil or a list of attributes
  (defun xml-isattributelist (attributes)
    (or
     (endp attributes)
     (and
      (true-listp attributes)
      (xml-isattribute (car attributes))
      (xml-isattributelist (cdr attributes)))))
  
  (mutual-recursion
   ;xml-isnode (node) → returns true iff node is actually a node
   (defun xml-isnode (node)
     (and
      (true-listp node)
      (equal (length node) 3)
      (or
       (and
        (equal 'text (car node))
        (null (cadr node))
        (stringp (caddr node)))
       (and
        (stringp (car node))
        (xml-isattributelist (cadr node))
        (xml-isnodelist (caddr node))))))
   
   ;xml-isnodelist (nodes) → returns true iff nodes is a list of nodes
   (defun xml-isnodelist (nodes)
     (and
      (true-listp nodes)
      (or
       (endp nodes)
       (and
        (xml-isnode (car nodes))
        (xml-isnodelist (cdr nodes))))))
   
   )

  ;gluekids (nodes)→ Given a list of nodes, glue all nodes' children
  ;   together in one big list; i.e., if the nodes are rooted in some tree
  ;   where they're at depth k, then take all of the nodes at depth k+1
  ;   (cousins or siblings to one another), and put them into a list
  ;   together.
  (defun gluekids (nodes)
    (if (consp nodes)
      (let* ((node (car nodes))
             (kids (caddr node)))
              (concatenate 'list kids (gluekids (cdr nodes))))
      nil))

  ;xml-bfsfindnodes (nodes nodename) → returns a list of children nodes of type
  ;    nodename using BFS search that are at the shallowest depth.
  (defun xml-bfsfindnodes (nodes nodename)
    ; Build a dummy node that looks like xmlminidom, so we can act like
    ; nodes are rooted there as children, and we can just use xml-getnodes
    ; on it.
    (let* ((dummyroot (list "dummyroot" nil nodes))
           (maybes (xml-getnodes dummyroot nodename)))
        (if maybes
            ; We found them
            maybes
            ; There were no nodes.  Look deeper.
            (xml-bfsfindnodes (gluekids nodes) nodename))))

  (export IminidomParser))
