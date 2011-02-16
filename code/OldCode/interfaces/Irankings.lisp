;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.ss" ("cce" "dracula.plt") "modular" "lang")
(require "Ixmlminidom.lisp")

(defconst *rktablehead*
  (concatenate 'string
  "<table class=\"results\">"
  "<tr>"
  "<th rowspan=\"2\">Pair No.</th>"
  "<th rowspan=\"2\">Players</th>"
  "<th rowspan=\"2\">Strat / Flight</th>"
  "<th colspan=\"3\">Section Rank</th>"
  "<th colspan=\"3\">Overall Rank</th>"
  "<th rowspan=\"2\">Matchpoint Score</th>"
  "<th rowspan=\"2\">Percentage Score</th>"
  "<th rowspan=\"2\">Masterpoint Award</th>"  
  "</tr>"
  "<tr>"
  "<th>A</th>"
  "<th>B</th>"
  "<th>C</th>"
  "<th>A</th>"
  "<th>B</th>"
  "<th>C</th>"
  "</tr>"
  ))
(defconst *rktabletail*
  "</table>")

(interface Irankings
  (include Ixmlminidom)
  (sig getmatchpointtotal (node))
  (sig sortcontestants (unsortedcontestants))
  (sig sortmvranks (ranks mvs))
  (sig serializedrankings (rankingnodes))
  (sig serializedrankingsheader (gamenodes))
  (sig getcontestants (section dir id sections))
  (sig getcontestantsnames (contestants))
  (sig getrankstring (ranktype contestants))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;Contracts
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (con getmatchpointtotal-returns-real
    (implies (xml-isnode mpnode)
             (real (getmatchpointtotal mpnode))))
  (con sortcontestants-returns-list
    (implies (xml-isnodelist rankingnodes)
             (listp (sortcontestants rankingnodes))))
  (con sortmvranks-returns-three
    (implies (xml-isnodelist rankingnodes)
             (eq 3 (len (sortmvrankings rankingnodes (mv nil nil nil))))))
  (con serializedrankings-delivers-string
    (implies (xml-isnodelist rankingnodes)
             (stringp (serializedrankings rankingnodes))))
  (con getcontestants-delivers-xml-node
    (implies (and (stringp section)
                  (stringp dir)
                  (stringp id)
                  (xml-isnodelist sections))
             (xml-isnode (getcontestants section dir id sections))))
  (con getcontestantsnames-delivers-string
    (implies (xml-isnode contestants)
             (stringp (getcontestantsnames contestants))))
  (con getrankstring-delivers-string
    (implies (and (xml-isnode contestants)
             (stringp ranktype)
             (or (string-equal ranktype "Section")
                 (string-equal ranktype "Overall")))
             (stringp (getrankstring ranktype contestants)))))
