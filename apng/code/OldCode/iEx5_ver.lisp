#| Edward Flick
   Software Engineering I
   iEx5_ver

   Theorems for the iEx5 assignment.
|#

(in-package "ACL2")
(include-book "testing" :dir :teachpacks)
(include-book "doublecheck" :dir :teachpacks)
(include-book "iEx5")

; No theorems execute correctly becuase xmlminidom does not admit
(defthm getresults-nil=string-append-prefix-postfix-thm
  (implies (and (stringp prefix) (stringp postfix))
           (string-equal (getresults nil prefix postfix)
                         (string-append prefix postfix))))

(defthm getresults-nil-returns-a-string-thm
  (implies (and (stringp prefix) (stringp postfix))
           (stringp (getresults nil prefix postfix))))

