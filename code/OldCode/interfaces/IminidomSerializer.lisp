;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#| Team Steele
   Software Engineering I
   IminidomSerializer

   Software that creates a document object model from XML input, or
   creates XML from DOM input.
|#

(defconst *whitespace*
  (list (code-char 32)
        (code-char 10)
        (code-char 9)
        (code-char 11)
        (code-char 12)
        (code-char 13)
        (code-char 27)))
(defconst *endtagname* (cons #\> (cons #\/ *whitespace*)))
(defconst *endattrname* (cons #\= *whitespace*))
  
(interface IminidomSerializer
  ;xml-escape (unescapedchars) → returns string with bad chars replaced
  ;    with entities
  (sig xml-escape (unescapedchars))
  
  ;xml-serialize-attributes (attributes) → Returns a string that is
  ;    xml that represents the passed in attribute list.
  (sig xml-serialize-attributes (attributes))           
  
  ;xml-serizlize-nodes (xmlnodes) → Returns a string containing xml
  ;    nodes that represents the node list, xmlnodes.
  (sig xml-serialize-nodes (xmlnodes))

  ;xml-serizlize-dom (xmlnode) → Returns a string containing an xml
  ;    document that represents the dom passed in through xmlnode.
  (sig xml-serialize-dom (xmlnode))

  ;xml-unescape (escapedchars) → string with entities replaced
  (sig xml-unescape (escapedchars))
  
  ;xml-readnodeproperties (xmlchars) →
  ; returns (mv attributes remainingxmlstring)
  (sig xml-readnodeproperties (xmlchars))

  ;xml-skipdontcares (xmlchars) → returns next xmlchars sans don't cares
  (sig xml-skipdontcares (xmlchars))

  ;xml-readnodes (xmlchars) → returns (mv nodes remainingxmlstring)
  (sig xml-readnodes (xmlchars))
  
  ;xml-readnode (xmlchars) → returns the root node from xmlstring
  (sig xml-readnode (xmlchars))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;Contracts
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (con xml-unescape-returns-string
       (implies (standard-char-listp x)
                (stringp (xml-unescape x))))
  (con xml-skipdontcares-lessthanequal-xmlchars
       (imlies (and (standard-char-listp x)
                    (equal (length x) y))
               (<= (xml-skipdontcares x) y)))
  (con xml-readnode-serialize-dom-invertible
       (implies (xml-isnode x)
                (equal x (xml-readnode (xml-serialize-dom x)))))
)
