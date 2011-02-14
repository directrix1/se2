;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.ss" ("cce" "dracula.plt") "modular" "lang")
#| Team Steele
   Software Engineering I
   iio

|#


(interface Iio

  ;main(bridgeXML state) Given a duplicate bridge XML file, extracts
  ;appropriate information to create four 
  ;HTML pages that link together: boards, boards with travelers, rankings,
  ;and personal score card webpages.
  (sig main (bridgeXML state))
  
  ;boards-no-trav(bridgeXML state) Given a duplicate bridge XML file, gets
  ;the following information from each board listed in the XML file and 
  ;creates an HTML page with this information formatted into tables:
  ;Board numbers, dealer, vulnerable, and hands for each direction
  (sig boards-no-trav (bridgeXML state))
  
  
  ;boards-trav(bridgeXML state) Given a duplicate bridge XML file, gets the
  ;same information as boards-no-trav as well as the travelers information 
  ;for each board, which includes the total score and matchpoints for all 
  ;pairs at that board. Creates HTML page with this information with the 
  ;board information and travelers information in separate tables.
  (sig boards-trav (bridgeXML state))
  
  
  ;rankings(bridgeXML state) Given a duplicate bridge XML file, creates a 
  ;rankings table in an HTML file including each pairs ranking and various 
  ;other stats such as matchpoint and percentage score.
  (sig rankings (bridgeXML state))
  
  
  ;personal-score-cards(bridgeXML state) Given a duplicate bridge XML file,
  ;creates an HTML file containing personal score cards for each pair, which
  ;includes information about each match they played an against whom.
  (sig personal-score-cards (bridgeXML state))
  
  )

 
