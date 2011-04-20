;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
(require "IQRCode.lisp")
(require "MMatrix.lisp")

(module MQRCode
  
  (import IMatrix)
  
  (defun matrix->text (matrix)
    nil
    )
  
  ;Get-Int-Value-for-char
  ;Parameter: The char you want an int value for
  ;Returns: The Associated int value
  ;C->12
  (defun Get-Int-Value-For-Char (Char)
    (case Char
      (#\0 0) (#\1 1) (#\2 2) (#\3 3) (#\4 4) (#\5 5) (#\6 6) (#\7 7) (#\8 8)
      (#\9 9) (#\A 10)(#\B 11)(#\C 12)(#\D 13)(#\E 14)(#\F 15)(#\G 16)(#\H 17)
      (#\I 18)(#\J 19)(#\K 20)(#\L 21)(#\M 22)(#\N 23)(#\O 24)(#\P 25)(#\Q 26)
      (#\R 27)(#\S 28)(#\T 29)(#\U 30)(#\V 31)(#\W 32)(#\X 33)(#\Y 34)(#\Z 35)
      (#\SPACE 36)    (#\$ 37)(#\% 38)(#\* 39)(#\+ 40)(#\- 41)(#\. 42)(#\/ 43)
      (#\: 44)
      )
    )
  
  (defun Get-Char-Value-For-Int (Int)
    (case Int
      (0 #\0) (1 #\1) (2 #\2) (3 #\3) (4 #\4) (5 #\5) (6 #\6) (7 #\7) (8 #\8)
      (9 #\9) (10 #\A)(11 #\B)(12 #\C)(13 #\D)(14 #\E)(15 #\F)(16 #\G)(17 #\H)
      (18 #\I)(19 #\J)(20 #\K)(21 #\L)(22 #\M)(23 #\N)(24 #\O)(25 #\P)(26 #\Q)
      (27 #\R)(28 #\S)(29 #\T)(30 #\U)(31 #\V)(32 #\W)(33 #\X)(34 #\Y)(35 #\Z)
      (36 #\SPACE)    (37 #\$)(38 #\%)(39 #\*)(40 #\+)(41 #\-)(42 #\.)(43 #\/)
      (44 #\/)
      )
    )
  
  (defun Int->Char-Pair (int)
    (cons (Get-Char-Value-For-Int (floor int 45)) (Get-Char-Value-For-Int (mod int 45)))
    )
  
  
  ;Char-Pair->Int
  ;Parameters: a b - two chars to add together (45*a + b)
  ;Returns: int value for (45*a + b)
  ;'(A B)->461
  (defun Char-Pair->Int (a b)
    (+ (* 45 (Get-Int-Value-For-Char a)) 
       (Get-Int-Value-For-Char b))
    )
  
  ;Char-List->Int-List-h
  ;helper function for String->Int-List
  ;Parameters: Char-List - list of chars to convert to ints
  ;             Int-List - The return value that gets added to
  ;                         as we go through the Char-List
  ;Returns: list of integers associated with the paired characters
  (defun Char-List->Int-List-h (Char-List Int-List)
    (if (endp Char-List) Int-List
        (if (= 1 (length Char-List))
            (Char-List->Int-List-h (cddr Char-List) (append Int-List (list (Char-Pair->Int (car Char-List) #\SPACE))))
            
            (Char-List->Int-List-h (cddr Char-List) (append Int-List (list (Char-Pair->Int (car Char-List) (cadr Char-List)))))))
    )
  
  (defun Int-List->Char-List-h (Int-List Char-List)
    (if (endp Int-List) Char-List
        (Int-List->Char-List-h  (cdr Int-List) (append Char-List (list (Int->Char-Pair  (car Int-List))))))
    )
  
  ;String->Int-List
  ;Parameters: Char-List - list of chars to convert to ints
  ;Returns: list of integers associated with the paired characters
  (defun Char-List->Int-List (Char-Pairs)
    (Char-List->Int-List-h Char-Pairs nil)
    )
  
  
  ;Char-List->Int-List
  ;Parameters: Char-List - list of char-pairs to convert to ints
  ;Returns: list of integers associated with the paired characters
  ;'((A B) (A C)) -> '(461 462)
  (defun Char-List->Int-List2 (Char-Pairs)
    (if (endp Char-Pairs)
        nil
        (cons (Char-Pair->Int (car Char-Pairs) (cadr Char-Pairs)) (Char-List->Int-List (cddr Char-Pairs))))
    )
  
  ;(char-pair->char-list '((#\A . #\B) (#\c . #\d) (#\e . #\f)))
  ;'(#\A #\B #\c #\d #\e #\f)
  (defun char-pair->char-list (char-pairs)
    (if (endp char-pairs)
        nil
        (if (= 1 (length char-pairs))
            (append (list(caar char-pairs)) (list(cdar char-pairs)))
            (append (list (caar char-pairs)) (list(cdar char-pairs)) (list(caadr char-pairs)) (list(cdadr char-pairs))  (char-pair->char-list (cddr char-pairs)))))
    )
  
  ;(Int-List->Char-List '(461 462 463))
  ;'((#\A . #\B)
  ;  (#\A . #\C)
  ;  (#\A . #\D))
  (defun Int-List->Char-List (Int-List)
    (Int-List->Char-List-h Int-List nil)
    )
  
  ;String->Int-List
  ;Parameters: Char-List - list of char-pairs to convert to ints
  ;Returns: list of integers associated with the paired characters
  ;'((A B) (A C)) -> '(461 462)
  (defun String->Int-List (String)
    (Char-List->Int-List (coerce (string-upcase String) 'LIST))
    )
  
  
  ;(Int-List->String '(461 462 463))
  ;"ABACAD"
  (defun Int-List->String (Int-List)
    (coerce (char-pair->char-list (Int-List->Char-List Int-List)) 'STRING)
    )
  
  
  ; Indicates what mode the QR code is
  (defconst *NUMERIC-MODE* '(0 0 0 1))
  (defconst *ALPHA-MODE* '(0 0 1 0))
  (defconst *8BIT-MODE* '(0 1 0 0))
  (defconst *KANJI* '(1 0 0 0))
  
  ; Converts an integer to a binary. Returns
  ; a list of characters with 0s or 1s representing
  ; the integer. For example, (int->bin 5) would return
  ; (1 0 1)
  (defun int->bin-help (int)
    (if (not (posp int))
        nil
        (cons (mod int 2) (int->bin-help (floor int 2)))))
  
  (defun int->bin (int)
    (reverse (int->bin-help int)))
  
  ; Converts a binary list to an integer. Takes in a list like
  ; (0 1 0 1) and returns an integer like 5.
  ; (expt r i) = r^i
  (defun bin->int (binary)
    (if (not (posp (len binary)))
        0
        (+ (* (car binary) (expt 2 (- (len binary) 1)))
           (bin->int (cdr binary)))))
  
  
  
  ; Pads specified amount of zeros in front of given binary string
  (defun pad-0s-front (binary numberOf0s)
    (if (not (posp numberOf0s))
        binary
        (append (list 0) (pad-0s-front binary (- numberOf0s 1)))))
  
  ; Pads specified amount of zeros at the end of given binary string
  (defun pad-0s-back (binary numberOf0s)
    (if (not (posp numberOf0s))
        binary
        (append (pad-0s-back binary (- numberOf0s 1)) (list 0))))
  
  ; Returns the binary string for the number
  ; of characters. This is part of the QR header.
  (defun char-count (chrs)
    (let* ((binary (int->bin (len chrs))))
      (if (< (len binary) 9)
          (pad-0s-front binary (- 9 (len binary)))
          binary)))
  
  ; Returns the QR header for alphanumeric mode given a list of characters
  (defun get-header (chrs)
    (append *ALPHA-MODE* (char-count chrs)))
  
  ; Returns the QR header for alphanumeric mode given a String
  (defun get-header-string (String)
    (get-header (coerce (string-upcase String) 'LIST))
    )
  
  ;pads the front of a binary list to make it's length 11
  (defun make-11-digit-bin (bin)
    (pad-0s-front bin (- 11 (length bin)))
    )
  
  ;makes a list of integers into a binary string
  (defun Int-List->Bin-h (Int-List Bin)
    (if (= 0 (length Int-List))
        Bin
        (Int-List->Bin-h (cdr Int-List) (append Bin (make-11-digit-bin (int->bin (car Int-List))))))
    )
  
  (defun Int-List->Bin (Int-List)
    (Int-List->Bin-h Int-List nil)
    )
  
  ;(defun Int-List->Bin (Int-List)
  ; (if (= 0 (length Int-List))
  ;  nil
  ;	(cons (make-11-digit-bin (int->bin (car Int-List))) (Int-List->Bin (cdr Int-List)))))
  
  (defun String->binary (String)
    (Int-List->Bin (String->Int-List String))
    )
  
  (defun pad-back-if-not-divisible-by-eight (bin)
    (if (integerp (/ (length bin) 8))
        bin
        (pad-back-if-not-divisible-by-eight (pad-0s-back bin 1))
        )
    )
  
  ;delimits a binary string into lists of size 8
  (defun list-of-bin->8-digit-lists-h (bin bin-lists)
    (if (endp bin)
        bin-lists
        (list-of-bin->8-digit-lists-h (cddddr (cddddr bin)) (append bin-lists 
                                                                    (list (list (car bin)
                                                                                (cadr bin)
                                                                                (caddr bin)
                                                                                (cadddr bin)
                                                                                (cadddr (cdr bin))
                                                                                (cadddr (cddr bin))
                                                                                (cadddr (cdddr bin))
                                                                                (cadddr (cddddr bin))
                                                                                ))))))
  
  ;delimits a binary string into lists of size 8
  (defun list-of-bin->8-digit-lists2 (bin)
    ;length of bin should be divisible by 8
    (if (not (integerp (/ (length bin) 8)))
        nil
        (list-of-bin->8-digit-lists-h bin nil))
    )
  (defun list-of-bin->8-digit-lists (bin)
    ;length of bin should be divisible by 8
    (if (not (integerp (/ (length bin) 8)))
        nil
        (if (endp bin)
            nil
            (cons (list (car bin)
                        (cadr bin)
                        (caddr bin)
                        (cadddr bin)
                        (cadddr (cdr bin))
                        (cadddr (cddr bin))
                        (cadddr (cdddr bin))
                        (cadddr (cddddr bin)))
                  (list-of-bin->8-digit-lists (cddddr (cddddr bin)))))))
  
  
  (defun bin->int-lists-h (bin-list)
    (if (endp bin-list)
        nil
        (append (bin->int (car bin-list)) (list (bin->int-lists-h bin-list))))
    )
  ;need to pad front with 0's I think
  (defun bin->int-lists (bin)
    (bin->int-lists-h (list-of-bin->8-digit-lists bin))
    )
  
  ;adds the data postfix if the capacity is larger than the number
  ;of code words
  (defun data-postfix (length max-length)
    (if (>= length max-length)
        nil
        (if (>= (- max-length length) 2)
            (cons '(1 1 1 0 1 1 0 0) (cons '(0 0 0 1 0 0 0 1)
                                           (data-postfix (+ length 2) max-length)))
            (cons '(1 1 1 0 1 1 0 0)
                  (data-postfix (+ length 1) max-length)))))
  
  ;append header to string all in binary and pad 4 0 to back
  (defun get-entire-binary-string (String)
    (let* ((code-words (list-of-bin->8-digit-lists
                        (pad-back-if-not-divisible-by-eight
                         (pad-0s-back (append (get-header-string String)
                                              (String->binary String)) 4)))))
      (append code-words (data-postfix (length code-words) 9)))
    )
  
  ;converts a list of binary strings into a list of integers
  (defun bin-list->int-list (binary)
    (if (endp binary)
        nil
        (cons (bin->int (car binary))
              (bin-list->int-list (cdr binary)))))
  
  
  ;This puts us at "2.2.5 Encode to code words" on the qr code site we're using
  ;(get-entire-binary-string "abcde123")
  
  (defun Get-Smallest-Int-From-List-h (int int-list)
    (if (= 1 (length int-list))
        (if (< int (car int-list))
            int
            (car int-list)
            )
        (if (< int (car int-list))
            (Get-Smallest-Int-From-List-h int (cdr int-list))
            (Get-Smallest-Int-From-List-h (car int-list) (cdr int-list))
            )
        )
    )
  
  
  (defun Get-Smallest-Int-From-List (int-list)
    (Get-Smallest-Int-From-List-h (+ 1 (car int-list)) int-list)
    )
  
  
  
  (defun Get-1st-ndx-of-int (int int-list)
    (if (= 0 (length int-list)) -9999999
        (if (= int (car int-list))
            0
            (+ 1 (Get-1st-ndx-of-int int (cdr int-list)))
            )
        )
    )
  
  (defun Get-1st-ndx-of-smallest (int-list)
    (Get-1st-ndx-of-int (Get-Smallest-Int-From-List int-list) int-list) 
    )
  
  (defun Get-nth (n list)
    (if (> n (length list))
        nil
        (if (= 0 n)
            (car list)
            (Get-nth (- n 1) (cdr list))
            )
        )
    )
  
  ;No. of modules in a row or column = (5 + i)
  ;return i
  ;3 + 
  (defun penalty1val (matrix)
    1
    )
  
  ;block sizes (m - 1) * (n - 1)
  (defun penalty2val (matrix)
    1
    )
  
  ;# of times we get 1 0 1 1 1 0 1
  (defun penalty3val (matrix)
    1
    )
  
  ;(%of black - 50) / 5
  ;so 70% black should return 4
  (defun penalty4val (matrix)
    1
    )
  
  (defun get-penalty (matrix)
    ;(+ (+ 3 (penalty1val matrix))
    ;(+ (* 3 (penalty2val matrix))
    ;(+ (* 40 (penalty3val matrix))
    ;(* 10 (penalty4val matrix)))))
    (+ (penalty1val matrix)
       (+ (penalty2val matrix)
          (+ (penalty3val matrix)
             (penalty4val matrix))))
    )
  
  (defun get-penaltys (list-matrix)
    (if (= 0 (length list-matrix))
        nil
        (append (get-penaltys (cdr list-matrix)) (get-penalty (car list-matrix)))
        )
    )
  
  (defun get-smallest-penalty-matrix (matricies)
    (Get-nth (Get-1st-ndx-of-smallest (get-penaltys matricies)) matricies)
    )
  
  (defun matrix ()
    '((1 2 3 4)
      (4 5 6 4)
      (3 2 1 4)
      (3 2 8 8)
      (2 3 3 3))
    )
  
  (defun ndx-from-matrix-to-list-h (matrix ndx lst nth)
    (if (> ndx (length matrix))
        (reverse (cdr lst))
        (ndx-from-matrix-to-list-h matrix (+ 1 ndx)
                                   (append (list (get-nth nth (get-nth ndx matrix))) lst) nth ))
    )
  
  (defun ndx-from-matrix-to-list (matrixIN matrixOUT ndx)
    (if (> ndx (length (car matrixIN)))
        (reverse (cdr (reverse matrixOUT)))
        (ndx-from-matrix-to-list
         matrixIN
         (append matrixOUT (list (ndx-from-matrix-to-list-h matrixIN 0 nil ndx)))
         (+ 1 ndx)))
    )
  
  (defun transpose (matrix)
    (ndx-from-matrix-to-list matrix nil 0)
    )
  
  (defconst *1-L* (list (list 0 87 229 146 149 238 102 21) 19))
  (defconst *1-M* (list (list 0 251 67 46 61 118 70 64 94 32 45) 16))
  (defconst *1-Q* (list (list 0 74 152 176 100 86 100 106 104 130 218 206 140 78) 13))
  (defconst *1-H* (list (list 0 43 139 206 78 43 239 123 206 214 147 24 99 150 39 243 163 136) 9))
  (defconst *2-L* (list (list  0 87 229 146 149 238 102 21) 34))
  (defconst *2-M* (list (list 0 120 104 107 109 102 161 76 3 91 191 147 169 182 192 225 120) 28))
  (defconst *2-Q* (list (list 0 210 171 247 242 93 230 14 109 221 53 200 74 8 172 98 80 219 134 160 105 165 231) 22))
  (defconst *2-H* (list (list 0 168 223 200 104 224 234 108 180 110 190 195 147 205 27 232 201 21 43 245 87 42 195 212 119 242 37 9 123) 16))
  ;Table for converting an exponent to its codded int value
  (defun Get-Int-Value-For-Exponent (int)
    (if (> int 255)
        (Get-Int-Value-For-Exponent (- int 255))
        (case int
          (0 1)(1 2) (2 4) (3 8) (4 16) (5 32) (6 64) (7 128) (8 29) (9 58) (10 116) (11 232)
          (12 205) (13 135) (14 19) (15 38) (16 76) (17 152) (18 45) (19 90) (20 180) (21 117)
          (22 234) (23 201) (24 143) (25 3) (26 6) (27 12) (28 24) (29 48) (30 96) (31 192) (32 157)
          (33 39) (34 78) (35 156) (36 37) (37 74) (38 148) (39 53) (40 106) (41 212) (42 181) (43 119)
          (44 238) (45 193) (46 159) (47 35) (48 70) (49 140) (50 5) (51 10) (52 20) (53 40) (54 80)
          (55 160) (56 93) (57 186) (58 105) (59 210) (60 185) (61 111) (62 222) (63 161) (64 95)
          (65 190) (66 97) (67 194) (68 153) (69 47) (70 94) (71 188) (72 101) (73 202) (74 137) 
          (75 15) (76 30) (77 60) (78 120) (79 240) (80 253) (81 231) (82 211) (83 187) (84 107) 
          (85 214) (86 177) (87 127) (88 254) (89 225) (90 223) (91 163) (92 91) (93 182) (94 113)
          (95 226) (96 217) (97 175) (98 67) (99 134) (100 17) (101 34) (102 68) (103 136) (104 13)
          (105 26) (106 52) (107 104) (108 208) (109 189) (110 103) (111 206) (112 129) (113 31)
          (114 62) (115 124) (116 248) (117 237) (118 199) (119 147) (120 59) (121 118) (122 236)
          (123 197) (124 151) (125 51) (126 102) (127 204) (128 133) (129 23) (130 46) (131 92) 
          (132 184) (133 109) (134 218) (135 169) (136 79) (137 158) (138 33) (139 66) (140 132)
          (141 21) (142 42) (143 84) (144 168) (145 77) (146 154) (147 41) (148 82) (149 164) (150 85)
          (151 170) (152 73) (153 146) (154 57) (155 114) (156 228) (157 213) (158 183) (159 115)
          (160 230) (161 209) (162 191) (163 99) (164 198) (165 145) (166 63) (167 126) (168 252)
          (169 229) (170 215) (171 179) (172 123) (173 246) (174 241) (175 255) (176 227) (177 219)
          (178 171) (179 75) (180 150) (181 49) (182 98) (183 196) (184 149) (185 55) (186 110) (187 220)
          (188 165) (189 87) (190 174) (191 65) (192 130) (193 25) (194 50) (195 100) (196 200) (197 141)
          (198 7) (199 14) (200 28) (201 56) (202 112) (203 224) (204 221) (205 167) (206 83) (207 166)
          (208 81) (209 162) (210 89) (211 178) (212 121) (213 242) (214 249) (215 239) (216 195) (217 155)
          (218 43) (219 86) (220 172) (221 69) (222 138) (223 9) (224 18) (225 36) (226 72) (227 144) 
          (228 61) (229 122) (230 244) (231 245) (232 247) (233 243) (234 251) (235 235) (236 203)
          (237 139) (238 11) (239 22) (240 44) (241 88) (242 176) (243 125) (244 250) (245 233) (246 207)
          (247 131) (248 27) (249 54) (250 108) (251 216) (252 173) (253 71) (254 142) (255 1) 
          )))
  
  ;table for converting an int to its codded exponet value
  (defun Get-Exponet-Value-For-int (int)
    (case int
      (1 0)(2 1) (4 2) (8 3) (16 4) (32 5) (64 6) (128 7) (29 8) (58 9) (116 10) (232 11)
      (205 12) (135 13) (19 14) (38 15) (76 16) (152 17) (45 18) (90 19) (180 20) (117 21)
      (234 22) (201 23) (143 24) (3 25) (6 26) (12 27) (24 28) (48 29) (96 30) (192 31) (157 32)
      (39 33) (78 34) (156 35) (37 36) (74 37) (148 38) (53 39) (106 40) (212 41) (181 42) (119 43)
      (238 44) (193 45) (159 46) (35 47) (70 48) (140 49) (5 50) (10 51) (20 52) (40 53) (80 54)
      (160 55) (93 56) (186 57) (105 58) (210 59) (185 60) (111 61) (222 62) (161 63) (95 64) (190 65)
      (97 66) (194 67) (153 68) (47 69) (94 70) (188 71) (101 72) (202 73) (137 74) (15 75) (30 76)
      (60 77) (120 78) (240 79) (253 80) (231 81) (211 82) (187 83) (107 84) (214 85) (177 86) (127 87)
      (254 88) (225 89) (223 90) (163 91) (91 92) (182 93) (113 94) (226 95) (217 96) (175 97) (67 98)
      (134 99) (17 100) (34 101) (68 102) (136 103) (13 104) (26 105) (52 106) (104 107) (208 108)
      (189 109) (103 110) (206 111) (129 112) (31 113) (62 114) (124 115) (248 116) (237 117) (199 118)
      (147 119) (59 120) (118 121) (236 122) (197 123) (151 124) (51 125) (102 126) (204 127) (133 128)
      (23 129) (46 130) (92 131) (184 132) (109 133) (218 134) (169 135) (79 136) (158 137) (33 138)
      (66 139) (132 140) (21 141) (42 142) (84 143) (168 144) (77 145) (154 146) (41 147) (82 148) 
      (164 149) (85 150) (170 151) (73 152) (146 153) (57 154) (114 155) (228 156) (213 157) (183 158)
      (115 159) (230 160) (209 161) (191 162) (99 163) (198 164) (145 165) (63 166) (126 167) (252 168)
      (229 169) (215 170) (179 171) (123 172) (246 173) (241 174) (255 175) (227 176) (219 177) (171 178)
      (75 179) (150 180) (49 181) (98 182) (196 183) (149 184) (55 185) (110 186) (220 187) (165 188)
      (87 189) (174 190) (65 191) (130 192) (25 193) (50 194) (100 195) (200 196) (141 197) (7 198) 
      (14 199) (28 200) (56 201) (112 202) (224 203) (221 204) (167 205) (83 206) (166 207) (81 208)
      (162 209) (89 210) (178 211) (121 212) (242 213) (249 214) (239 215) (195 216) (155 217) (43 218)
      (86 219) (172 220) (69 221) (138 222) (9 223) (18 224) (36 225) (72 226) (144 227) (61 228)
      (122 229) (244 230) (245 231) (247 232) (243 233) (251 234) (235 235) (203 236) (139 237) 
      (11 238) (22 239) (44 240) (88 241) (176 242) (125 243) (250 244) (233 245) (207 246) (131 247)
      (27 248) (54 249) (108 250) (216 251) (173 252) (71 253) (142 254) (1 255) 
      
      ))
  
  ;change a list of ints to their codded exponet value
  (defun list-int-to-exponet (int-list)
    (if(endp int-list)
       ()
       (cons(Get-Exponet-Value-For-int (car int-list))(list-int-to-exponet (cdr int-list)))
       )
    )
  
  ;change a list of exponet values to its codded int
  (defun list-exponet-to-int (exponet-list)
    (if(endp exponet-list)
       ()
       (cons(Get-Int-Value-For-Exponent (car exponet-list))(list-exponet-to-int (cdr exponet-list)))
       )
    )
  
  ;add the coefficent from the first fx to the list of gx coefficents
  (defun coefficent-addition(gx-List fx-1st-coef)
    (if(endp gx-List)
       ()
       (cons (+ (car gx-List) fx-1st-coef)
             (coefficent-addition (cdr gx-List) fx-1st-coef))))
  
  ;match the size of fx and gx
  (defun fx-fix (gx-List fx-List)
    (if(endp gx-List)
       ()
       (if(endp fx-List)
          (cons 0 (fx-fix (cdr gx-List) fx-List))
          (cons (car fx-List)(fx-fix (cdr gx-List) (cdr fx-List))))
       ))
  
  ;logical sum two lists
  (defun list-xor(gx-List fx-List)
    (if(endp gx-List)
       ()
       (cons (logxor (car gx-List) (car fx-List)) (list-xor(cdr gx-List) (cdr fx-List)))))
  
  
  ;determine error correction numbers
  ;Note this is the main function.
  ;Provide the coefficents for gx in a list and fx in a list and the count of data code words
  ;from the table
  (defun fprime(gx-List fx-List count)
    (let*(
          (first-exponet(Get-Exponet-Value-For-int(car fx-List)))
          (gx-change(coefficent-addition gx-List first-exponet))
          (gx(list-exponet-to-int gx-change))
          (fx(fx-fix (list-exponet-to-int gx-List) fx-List))
          (newCount(- count 1))
          (fprime-calc(cdr (list-xor gx fx))))
      (if(= count 0)
         fx-List
         (fprime gx-List fprime-calc newCount)
         )
      
      ))
  
  ;main function first paramater is version 2nd parm is numbers
  (defun error-correction(version fx-List)
    (let*(
          (gx-List(car version))
          (count(cadr version)))
      (append fx-List (fprime gx-List fx-List count))))
  
  ; version 1 is 21*21 matrix
  (defconst *CORNER-LENGTH* 6)  ; TECHNICALLY 7, BUT MINUS 1 FOR ZERO-INDEXING
  (defconst *MODULE-SIZE* 20)  ; TECHNICALLY 21, BUT MINUS 1 FOR ZERO-INDEXING
  
  ; The following function creates the square corners
  ; on the image. *CORNER-LENGTH* is what should get
  ; passed in for the length parameter
  (defun corner-markers (matrix length)
    (let* ((top-left1 (mset matrix 0 length 1))
           (top-left2 (mset top-left1 *CORNER-LENGTH* length 1))
           (top-left3 (mset top-left2 length 0 1))
           (top-left-fin (mset top-left3 length *CORNER-LENGTH* 1))
           (top-right1 (mset top-left-fin 0 (- *MODULE-SIZE* length) 1));top
           (top-right2 (mset top-right1 *CORNER-LENGTH* (- *MODULE-SIZE* length) 1));bottom
           (top-right3 (mset top-right2 length (- *MODULE-SIZE* *CORNER-LENGTH*) 1));left
           (top-right-fin (mset top-right3 length *MODULE-SIZE* 1))
           (bot-left1 (mset top-right-fin (- *MODULE-SIZE* *CORNER-LENGTH*) length 1));top
           (bot-left2 (mset bot-left1 *MODULE-SIZE* length 1));bot
           (bot-left3 (mset bot-left2 (- *MODULE-SIZE* length) 0 1));left
           (fin-matrix (mset bot-left3 (- *MODULE-SIZE* length) *CORNER-LENGTH* 1)))
      (if (>= length 1)
          (corner-markers fin-matrix (- length 1))
          fin-matrix)))
  
  ; puts the inner part of the corner markers
  (defun corner-inner (matrix length)
    (let* ((top-left1 (mset matrix 2 (+ length 2) 1))
           (top-left2 (mset top-left1 3 (+ length 2) 1))
           (top-left3 (mset top-left2 4 (+ length 2) 1))
           (top-right1 (mset top-left3 2 (- *MODULE-SIZE* (+ length 2)) 1))
           (top-right2 (mset top-right1 3 (- *MODULE-SIZE* (+ length 2)) 1))
           (top-right3 (mset top-right2 4 (- *MODULE-SIZE* (+ length 2)) 1))
           (bot-left1 (mset top-right3 (- *MODULE-SIZE* 2) (+ length 2) 1))
           (bot-left2 (mset bot-left1 (- *MODULE-SIZE* 3) (+ length 2) 1))
           (bot-left3 (mset bot-left2 (- *MODULE-SIZE* 4) (+ length 2) 1)))
      (if (>= length 1)
          (corner-inner bot-left3 (- length 1))
          bot-left3)))
  
  ; setup the timing pattern
  (defun zero-ltiming (matrix last-row)
    (if (> last-row (- *MODULE-SIZE* 7))
        matrix
        (zero-ltiming (mset matrix last-row 6 0)
                      (+ last-row 1))))
  (defun left-timing-pattern (matrix last-row)
    (if (> last-row (- *MODULE-SIZE* 7))
        matrix
        (left-timing-pattern (mset matrix last-row 6 1)
                             (+ last-row 2))))
  (defun zero-ttiming (matrix last-col)
    (if (> last-col (- *MODULE-SIZE* 7))
        matrix
        (zero-ttiming (mset matrix 6 last-col 0)
                      (+ last-col 1))))
  (defun top-timing-pattern (matrix last-col)
    (if (> last-col (- *MODULE-SIZE* 7))
        matrix
        (top-timing-pattern (mset matrix 6 last-col 1)
                            (+ last-col 2))))
  (defun zero-timing (matrix)
    (let* ((m1 (zero-ltiming matrix 8)))
      (zero-ttiming m1 8)))
  
  ;Mask Pattern Setup
  (defun mask-pattern (matrix)
    (let*((mask1(mset matrix 3 8 1))
          (mask2(mset mask1  8 17 1))
          (mask3(mset mask2  4 8 1))
          (mask4(mset mask3  8 16 1))
          (mask5(mset mask4  7 8 1))
          (mask6(mset mask5  8 14 1))
          (mask7(mset mask6  17 8 1))
          (mask8(mset mask7  8 3 1))
          (mask9(mset mask8  18 8 1))
          (mask10(mset mask9  8 2 1))
          (mask11(mset mask10 13 8 1))
          (mask12(mset mask11 1 8 0))
          (mask13(mset mask12 2 8 0))
          (mask14(mset mask13 5 8 0))
          (mask15(mset mask14 8 8 0))
          (mask16(mset mask15 0 8 0))
          (mask17(mset mask16 14 8 0))
          (mask18(mset mask17 15 8 0))
          (mask19(mset mask18 16 8 0))
          (mask20(mset mask19 19 8 0))
          (mask21(mset mask20 20 8 0))
          (mask22(mset mask21  8 0 0))
          (mask23(mset mask22  8 1 0))
          (mask24(mset mask23  8 4 0))
          (mask25(mset mask24  8 5 0))
          (mask26(mset mask25  8 7 0))
          (mask27(mset mask26  8 15 0))
          (mask28(mset mask27  8 18 0))
          (mask29(mset mask28  8 19 0))
          (mask30(mset mask29  8 20 0))
          (mask31(mset mask30  8 13 0)))
      mask31))
  
  ; To use, (zero-tl-corner matrix 0 0)
  (defun zero-tl-corner (matrix row column)
    (if (< row 8)
        (if (< column 8)
            (zero-tl-corner (mset matrix row column 0) row (+ column 1))
            (zero-tl-corner matrix (+ row 1) 0))
        matrix))
  
  ; To use, (zero-tr-corner matrix 0 13)
  (defun zero-tr-corner (matrix row column)
    (if (< row 8)
        (if (< column 21)
            (zero-tr-corner (mset matrix row column 0) row (+ column 1))
            (zero-tr-corner matrix (+ row 1) 0))
        matrix))
  
  ; To use, (zero-bl-corner matrix 13 0)
  (defun zero-bl-corner (matrix row column)
    (if (< row 21)
        (if (< column 8)
            (zero-bl-corner (mset matrix row column 0) row (+ column 1))
            (zero-bl-corner matrix (+ row 1) 0))
        matrix))
  
  (defun zero-corners (matrix)
    (let* ((m1 (zero-tl-corner matrix 0 0))
           (m2 (zero-tr-corner m1 0 13))
           (m3 (zero-bl-corner m2 13 0)))
      m3))
  
  ; Sets up the table with the corner markers, the timing pattern,
  ; and the format information
  (defun setup-table (matrix)
    (let* ((matrix0 (zero-corners matrix))
           (matrix1 (zero-timing matrix0))
           (matrix2 (corner-markers matrix1 *CORNER-LENGTH*))
           (matrix3 (corner-inner matrix2 2))
           ; add timing pattern, etc
           (matrix4 (left-timing-pattern matrix3 8))
           (matrix5 (top-timing-pattern matrix4 8))
           (matrix6 (mask-pattern matrix5)))
      matrix6))
  
  ; Defines an upwards-vertical block given the matrix and the row
  ; and column of the lower left corner of the block. Upwards means that
  ; the most significant bit is in the upper left corner, and moves in a 
  ; zig-zag motion to the lower right corner
  (defun up-vertical-block (matrix ll-row ll-col bit-str)
    (let* ((m1 (mset matrix (- ll-row 3) ll-col (car bit-str)))
           (m2 (mset m1 (- ll-row 3) (+ ll-col 1) (cadr bit-str)))
           (m3 (mset m2 (- ll-row 2) ll-col (third bit-str)))
           (m4 (mset m3 (- ll-row 2) (+ ll-col 1) (fourth bit-str)))
           (m5 (mset m4 (- ll-row 1) ll-col (fifth bit-str)))
           (m6 (mset m5 (- ll-row 1) (+ ll-col 1) (sixth bit-str)))
           (m7 (mset m6 ll-row ll-col (seventh bit-str)))
           (m8 (mset m7 ll-row (+ ll-col 1) (eighth bit-str))))
      m8))
  
  ; Defines a downwards-vertical block given the matrix and the row
  ; and column of the lower left corner of the block. Downwards means the
  ; most significant bit is in the lower left corner, and moves in a zig-zag
  ; motion up to the top right
  (defun down-vertical-block (matrix ll-row ll-col bit-str)
    (let* ((m1 (mset matrix ll-row ll-col (car bit-str)))
           (m2 (mset m1 ll-row (+ ll-col 1) (cadr bit-str)))
           (m3 (mset m2 (- ll-row 1) ll-col (third bit-str)))
           (m4 (mset m3 (- ll-row 1) (+ ll-col 1) (fourth bit-str)))
           (m5 (mset m4 (- ll-row 2) ll-col (fifth bit-str)))
           (m6 (mset m5 (- ll-row 2) (+ ll-col 1) (sixth bit-str)))
           (m7 (mset m6 (- ll-row 3) ll-col (seventh bit-str)))
           (m8 (mset m7 (- ll-row 3) (+ ll-col 1) (eighth bit-str))))
      m8))
  
  ; This represents an upwards special block (special meaning spanning across
  ; the timing pattern (see pg7 of the PDF specification)
  (defun up-special-block (matrix ll-row ll-col bit-str)
    (let* ((m1 (mset matrix (- ll-row 4) ll-col (car bit-str)))
           (m2 (mset m1 (- ll-row 4) (+ ll-col 1) (cadr bit-str)))
           (m3 (mset m2 (- ll-row 3) ll-col (third bit-str)))
           (m4 (mset m3 (- ll-row 3) (+ ll-col 1) (fourth bit-str)))
           (m5 (mset m4 (- ll-row 1) ll-col (fifth bit-str)))
           (m6 (mset m5 (- ll-row 1) (+ ll-col 1) (sixth bit-str)))
           (m7 (mset m6 ll-row ll-col (seventh bit-str)))
           (m8 (mset m7 ll-row (+ ll-col 1) (eighth bit-str))))
      m8))
  
  ; This represents a downwards special block (special meaning spanning across
  ; the timing pattern (see pg7 of the PDF specification)
  (defun down-special-block (matrix ll-row ll-col bit-str)
    (let* ((m1 (mset matrix ll-row ll-col (car bit-str)))
           (m2 (mset m1 ll-row (+ ll-col 1) (cadr bit-str)))
           (m3 (mset m2 (- ll-row 1) ll-col (third bit-str)))
           (m4 (mset m3 (- ll-row 1) (+ ll-col 1) (fourth bit-str)))
           (m5 (mset m4 (- ll-row 3) ll-col (fifth bit-str)))
           (m6 (mset m5 (- ll-row 3) (+ ll-col 1) (sixth bit-str)))
           (m7 (mset m6 (- ll-row 4) ll-col (seventh bit-str)))
           (m8 (mset m7 (- ll-row 4) (+ ll-col 1) (eighth bit-str))))
      m8))
  
  ; Inserts data blocks 1 to 12 into the matrix.
  (defun insert-data-block-1to12 (matrix bit-str)
    (let* ((m1 (up-vertical-block matrix *MODULE-SIZE* (- *MODULE-SIZE* 1) bit-str))
           (str1 (subseq bit-str 8 (len bit-str)))
           (m2 (down-vertical-block m1 (- *MODULE-SIZE* 4) (- *MODULE-SIZE* 1) str1))
           (str2 (subseq str1 8 (len str1)))
           (m3 (up-vertical-block m2 (- *MODULE-SIZE* 8) (- *MODULE-SIZE* 1) str2))
           (str3 (subseq str2 8 (len str2)))
           (m4 (down-vertical-block m3 (- *MODULE-SIZE* 8) (- *MODULE-SIZE* 3) str3))
           (str4 (subseq str3 8 (len str3)))
           (m5 (up-vertical-block m4 (- *MODULE-SIZE* 4) (- *MODULE-SIZE* 3) str4))
           (str5 (subseq str4 8 (len str4)))
           (m6 (down-vertical-block m5 *MODULE-SIZE* (- *MODULE-SIZE* 3) str5))
           (str6 (subseq str5 8 (len str5)))
           (m7 (up-vertical-block m6 *MODULE-SIZE* (- *MODULE-SIZE* 5) str6))
           (str7 (subseq str6 8 (len str6)))
           (m8 (down-vertical-block m7 (- *MODULE-SIZE* 4) (- *MODULE-SIZE* 5) str7))
           (str8 (subseq str7 8 (len str7)))
           (m9 (up-vertical-block m8 (- *MODULE-SIZE* 8) (- *MODULE-SIZE* 5) str8))
           (str9 (subseq str8 8 (len str8)))
           (m10 (down-vertical-block m9 (- *MODULE-SIZE* 8) (- *MODULE-SIZE* 7) str9))
           (str10 (subseq str9 8 (len str9)))
           (m11 (up-vertical-block m10 (- *MODULE-SIZE* 4) (- *MODULE-SIZE* 7) str10))
           (str11 (subseq str10 8 (len str10)))
           (m12 (down-vertical-block m11 *MODULE-SIZE* (- *MODULE-SIZE* 7) str11)))
      m12))
  
  ; Inserts data blocks 13 to 26. This part may also include the Error Correction words.
  (defun insert-data-block-13to26 (matrix bit-str)
    (let* ((m13 (up-vertical-block matrix *MODULE-SIZE* (- *MODULE-SIZE* 9) bit-str))
           (str12 (subseq bit-str 8 (len bit-str)))
           (m14 (down-vertical-block m13 (- *MODULE-SIZE* 4) (- *MODULE-SIZE* 9) str12))
           (str13 (subseq str12 8 (len str12)))
           (m15 (up-vertical-block m14 (- *MODULE-SIZE* 8) (- *MODULE-SIZE* 9) str13))
           (str14 (subseq str13 8 (len str13)))
           (m16 (down-special-block m15 (- *MODULE-SIZE* 12) (- *MODULE-SIZE* 9) str14))
           (str15 (subseq str14 8 (len str14)))
           (m17 (up-vertical-block m16 (- *MODULE-SIZE* 17) (- *MODULE-SIZE* 9) str15))
           (str16 (subseq str15 8 (len str15)))
           (m18 (down-vertical-block m17 (- *MODULE-SIZE* 17) (- *MODULE-SIZE* 11) str16))
           (str17 (subseq str16 8 (len str16)))
           (m19 (up-special-block m18 (- *MODULE-SIZE* 12) (- *MODULE-SIZE* 11) str17))
           (str18 (subseq str17 8 (len str17)))
           (m20 (down-vertical-block m19 (- *MODULE-SIZE* 8) (- *MODULE-SIZE* 11) str18))
           (str19 (subseq str18 8 (len str18)))
           (m21 (up-vertical-block m20 (- *MODULE-SIZE* 4) (- *MODULE-SIZE* 11) str19))
           (str20 (subseq str19 8 (len str19)))
           (m22 (down-vertical-block m21 *MODULE-SIZE* (- *MODULE-SIZE* 11) str20))
           (str21 (subseq str20 8 (len str20)))
           (m23 (up-vertical-block m22 (- *MODULE-SIZE* 8) (- *MODULE-SIZE* 13) str21))
           (str22 (subseq str21 8 (len str21)))
           (m24 (down-vertical-block m23 (- *MODULE-SIZE* 8) (- *MODULE-SIZE* 16) str22))
           (str23 (subseq str22 8 (len str22)))
           (m25 (up-vertical-block m24 (- *MODULE-SIZE* 8) (- *MODULE-SIZE* 18) str23))
           (str24 (subseq str23 8 (len str23)))
           (m26 (down-vertical-block m25 (- *MODULE-SIZE* 8) (- *MODULE-SIZE* 20) str24)))
      m26))
  
  ; Inserts the given bit string into the matrix
  (defun insert-data-blocks (matrix bit-str)
    (let* ((m1 (insert-data-block-1to12 matrix bit-str))
           (m2 (insert-data-block-13to26 m1 (subseq bit-str 96 (len bit-str)))))
      m2))
  
  (defconst *white* 0)
  (defconst *black* 1)
  

; mask 000 (i+j)%2 = 0
(defun mask000h2 (row N r# c# d)
  (if (zp c#) N
      (let* ((v (car row))
             (i (- d r#))
             (j (- d c#))
             (test (= (mod (+ i j) 2) 0))
             (nv (if test (if (equal v *white*) *black* *white*) v))
             (N++ (mset N i j nv)))
        (mask000h2 (cdr row) N++ r# (- c# 1) d))))

(defun mask000h1 (M N r# d)
  (if (zp r#) N
      (let* ((row (get-row M r# 0))
             (N+ (mask000h2 row N r# (len row) d)))
        (mask000h1 M N+ (- r# 1) d))))

(defun mask000 (M)
  (mask000h1 M (empty-matrix) (get-width M) (get-height M)))
;end

; mask 001 i%2 = 0
(defun mask001h2 (row N r# c# d)
  (if (zp c#) N
      (let* ((v (car row))
             (i (- d r#))
             (j (- d c#))
             (test (= (mod i 2) 0))
             (nv (if test (if (equal v *white*) *black* *white*) v))
             (N++ (mset N i j nv)))
        (mask001h2 (cdr row) N++ r# (- c# 1) d))))

(defun mask001h1 (M N r# d)
  (if (zp r#) N
      (let* ((row (get-row M r# 0))
             (N+ (mask001h2 row N r# (len row) d)))
        (mask001h1 M N+ (- r# 1) d))))

(defun mask001 (M)
  (mask001h1 M (empty-matrix) (get-width M) (get-height M)))
;end

; mask 010 j%3 = 0
(defun mask010h2 (row N r# c# d)
  (if (zp c#) N
      (let* ((v (car row))
             (i (- d r#))
             (j (- d c#))
             (test (= (mod j 3) 0))
             (nv (if test (if (equal v *white*) *black* *white*) v))
             (N++ (mset N i j nv)))
        (mask010h2 (cdr row) N++ r# (- c# 1) d))))

(defun mask010h1 (M N r# d)
  (if (zp r#) N
      (let* ((row (get-row M r# 0))
             (N+ (mask010h2 row N r# (len row) d)))
        (mask010h1 M N+ (- r# 1) d))))

(defun mask010 (M)
  (mask010h1 M (empty-matrix) (get-width M) (get-height M)))
;end

; mask 011 (i+j)%3 = 0
(defun mask011h2 (row N r# c# d)
  (if (zp c#) N
      (let* ((v (car row))
             (i (- d r#))
             (j (- d c#))
             (test (= (mod (+ i j) 3) 0))
             (nv (if test (if (equal v *white*) *black* *white*) v))
             (N++ (mset N i j nv)))
        (mask011h2 (cdr row) N++ r# (- c# 1) d))))

(defun mask011h1 (M N r# d)
  (if (zp r#) N
      (let* ((row (get-row M r# 0))
             (N+ (mask011h2 row N r# (len row) d)))
        (mask011h1 M N+ (- r# 1) d))))

(defun mask011 (M)
  (mask011h1 M (empty-matrix) (get-width M) (get-height M)))
;end

; mask 100 (i/2 + j/3)%2 = 0
(defun mask100h2 (row N r# c# d)
  (if (zp c#) N
      (let* ((v (car row))
             (i (- d r#))
             (j (- d c#))
             (test (= (mod (+ (/ i 2) (/ j 3)) 2) 0))
             (nv (if test (if (equal v *white*) *black* *white*) v))
             (N++ (mset N i j nv)))
        (mask100h2 (cdr row) N++ r# (- c# 1) d))))

(defun mask100h1 (M N r# d)
  (if (zp r#) N
      (let* ((row (get-row M r# 0))
             (N+ (mask100h2 row N r# (len row) d)))
        (mask100h1 M N+ (- r# 1) d))))

(defun mask100 (M)
  (mask100h1 M (empty-matrix) (get-width M) (get-height M)))
;end

; mask 101 (i*j)%2 + (i*j)%3 = 0
(defun mask101h2 (row N r# c# d)
  (if (zp c#) N
      (let* ((v (car row))
             (i (- d r#))
             (j (- d c#))
             (test (= (+ (mod (* i j) 2) (mod (* i j) 3)) 0))
             (nv (if test (if (equal v *white*) *black* *white*) v))
             (N++ (mset N i j nv)))
        (mask101h2 (cdr row) N++ r# (- c# 1) d))))

(defun mask101h1 (M N r# d)
  (if (zp r#) N
      (let* ((row (get-row M r# 0))
             (N+ (mask101h2 row N r# (len row) d)))
        (mask101h1 M N+ (- r# 1) d))))

(defun mask101 (M)
  (mask101h1 M (empty-matrix) (get-width M) (get-height M)))
;end

; mask 110 ((i*j)%2 + (i*j)%3)%2 = 0
(defun mask110h2 (row N r# c# d)
  (if (zp c#) N
      (let* ((v (car row))
             (i (- d r#))
             (j (- d c#))
             (test (= (mod (+ (mod (* i j) 2) (mod (* i j) 3)) 2) 0))
             (nv (if test (if (equal v *white*) *black* *white*) v))
             (N++ (mset N i j nv)))
        (mask110h2 (cdr row) N++ r# (- c# 1) d))))

(defun mask110h1 (M N r# d)
  (if (zp r#) N
      (let* ((row (get-row M r# 0))
             (N+ (mask110h2 row N r# (len row) d)))
        (mask110h1 M N+ (- r# 1) d))))

(defun mask110 (M)
  (mask110h1 M (empty-matrix) (get-width M) (get-height M)))
;end

; mask 111 ((i*j)%3 + (i+j)%2)%2 = 0
(defun mask111h2 (row N r# c# d)
  (if (zp c#) N
      (let* ((v (car row))
             (i (- d r#))
             (j (- d c#))
             (test (= (mod (+ (mod (* i j) 3) (mod (+ i j) 2)) 2) 0))
             (nv (if test (if (equal v *white*) *black* *white*) v))
             (N++ (mset N i j nv)))
        (mask111h2 (cdr row) N++ r# (- c# 1) d))))

(defun mask111h1 (M N r# d)
  (if (zp r#) N
      (let* ((row (get-row M r# 0))
             (N+ (mask111h2 row N r# (len row) d)))
        (mask111h1 M N+ (- r# 1) d))))

(defun mask111 (M)
  (mask111h1 M (empty-matrix) (get-width M) (get-height M)))
;end

  ; This is the main function of the module. Call this with the bit string passed in
  ; from the previous module and this inserts it into the matrix
  (defun qr-matrix (bit-str)
    (let* ((m1 (insert-data-blocks (empty-matrix) bit-str))
           (m2 (mask110 m1))
           (m3 (setup-table m2)))
      m3))  
  
  ; MAIN FUNCTION OF THE MODULE
  (defun text->matrix (text)
    (let* ((one (get-entire-binary-string text))
           (with-error (error-correction *1-H* (bin-list->int-list one)))
           (binary (Int-List->Bin with-error))
           (final (pad-0s-back binary (- 208 (len binary)))))
      (qr-matrix final)))
  
  (export IQRCode))