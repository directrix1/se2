;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#| Team Steele
   Software Engineering I
   IminidomParser

   Software that parses a minidom tree.
|#

(interface IminidomParser
  ;xml-text (text) → returns a text node with specified text.
  (sig xml-text (text))
  
  ;xml-node (nodetype attributes children) → returns a node with specified
  ;  nodetype attributes and children.
  (sig xml-node (nodetype attributes children))

  ;xml-attribute (attrname attrvalue) → returns an attribute with specified
  ;  name and value.
  (sig xml-attribute (attrname attrvalue))
  
  ;xml-getnodetype (node) → return the type of the node
  (sig xml-getnodetype (node))
  
  ;xml-getattrlist (node) → return the list of attributes of the node
  (sig xml-getattrlist (node))
  
  ;xml-getchildren (node) → return the children of the nodes
  (sig xml-getchildren (node))
  
  ;xml-getattrname (attribute) → return the name of the attribute
  (sig xml-getattrname (attribute))
  
  ;xml-getattrvalue (attribute) → return the value of the attribute
  (sig xml-getattrvalue (attribute))

  ;xml-getnodes (node nodename) → returns children of node with type
  ;  nodename
  (sig xml-getnodes (node nodename))
  
  ;xml-getdeepnodes (node nodename) → returns children of node with type
  ;  nodename searching recursively using DFS with node as root.
  (sig xml-getdeepnodes (node nodename))
  
  ;xml-getnode (node nodename) → returns first child node with type
  ;  nodename
  (sig xml-getnode (node nodename))
  
  ;xml-getdeepnode (node nodename) → returns first child node with type
  ;  nodename searching recursively using DFS with node as root.
  (sig xml-getdeepnode (node nodename))
  
  ;xml-getattribute (node attributename) → returns the value of node's
  ; attribute with name attributename
  (sig xml-getattribute (node attributename))
  
  ;xml-gettext (node) → returns the composite of all text inside of a node
  (sig xml-gettext (node))
  
  ;xml-isattribute (attribute) → returns true iff attribute is an mv of
  ;  length 2 with both elements of the mv being strings
  (sig xml-isattribute (attribute))
  
  ;xml-isattributelist (attributes) → returns true iff attributes
  ;  is nil or a list of attributes
  (sig xml-isattributelist (attributes))
  
  ;xml-isnode (node) → returns true iff node is actually a node
  (sig xml-isnode (node))
  
  ;xml-isnodelist (nodes) → returns true iff nodes is a list of nodes
  (sig xml-isnodelist (nodes))

  (sig xml-bfsfindnodes (nodes nodename))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;Contracts
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (con xml-getnodes-returns-nodes
       (implies (and
                 (stringp y)
                 (xml-isnode x))
                (xml-isnodelist (xml-getnodes y x))))
  (con xml-getnodes-returns-children
       (implies (and
                 (xml-isnode x)
                 (> (length (caddr x)) 0)
                 (equal node (car (caddr x)))
                 (equal y (car node))
                 (stringp y)
                 )
                (let ((res (xml-getnodes y x)))
                (and
                 (> (length res) 0)
                 (equal node (car res))))))
  (con xml-getdeepnodes-returns-nodes
       (implies (and
                 (stringp y)
                 (xml-isnode x))
                (xml-isnodelist (xml-getdeepnodes y x))))
  (con xml-getnode-returns-node-or-nil
       (implies (and
                 (xml-isnode x)
                 (stringp y))
                (let ((res (xml-getnode x y)))
                  (or
                   (null res)
                   (xml-isnode res)))))
  (con xml-getdeepnode-returns-node-or-nil
       (implies (and
                 (xml-isnode x)
                 (stringp y))
                (let ((res (xml-getdeepnode x y)))
                  (or
                   (null res)
                   (xml-isnode res)))))
  (con xml-getattribute-returns-string
       (implies (and
                 (xml-isnode x)
                 (stringp y))
                (stringp (xml-getattribute x y))))
  (con xml-gettext-returns-string
       (implies (xml-isnode x))
                (stringp (xml-gettext x)))
  
  (con xml-readnode-serialize-dom-invertible
       (implies (xml-isnode x)
                (equal x (xml-readnode (xml-serialize-dom x)))))
)
