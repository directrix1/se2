#| Team Steele
   Software Engineering II
   IapngExploder

   Takes an APNG file and explodes it into PNG files.
|#

(interface IapngExploder
  
  (sig explodeAPNG (apnginput, apngxml, pngnamebase, state))
  
  (sig chopFrame (apngfiledata))
  
  (sig makePNG (framedata))
  
  )