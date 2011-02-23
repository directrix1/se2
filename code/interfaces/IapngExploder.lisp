#| Team Steele
   Software Engineering II
   IapngExploder

   Takes an APNG file and explodes it into PNG files.
|#

(interface IapngExploder
  
  ;; Main function for breaking an APNG file into separate PNGs
  ;; apnginput is the string of the apng file data?
  ;; apngxml is the fCTL/aCTL data already parsed out?
  ;; pngnamebase is the name of the APNG file which will be used for the PNGs
  ;; state is state
  ;; Output is a list of mvs: (file-data, file-name)
  (sig explodeAPNG (apnginput, apngxml, pngnamebase, state))
  

  ;; This function pulls out one IDAT or FDAT chunk from the APNG file
  ;; apngfiledata is the string of the APNG's file data (how odd)
  ;; Output is a single frame's DAT as a string
  (sig chopFrame (apngfiledata))
  

  ;; This makes a full PNG file string out of a DAT chunk?
  (sig makePNG (framedata))
  
  )