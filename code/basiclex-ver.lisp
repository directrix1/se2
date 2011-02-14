(in-package "ACL2")
(include-book "basiclex")

(defthm split-at-delimiter-conserves-elements
  (implies (and (true-listp ds)
                (true-listp xs))
           (let* ((bfaf (split-at-delimiter ds xs))
                  (bf (car bfaf))
                  (af (cadr bfaf)))
             (equal (append bf af) xs))))
(defthm split-at-delimiter-prefix-contains-no-delimiters
  (implies (and (true-listp ds)
                (true-listp xs)
                (member-equal d ds))
           (not (member-equal d (car (split-at-delimiter ds xs))))))
(defthm split-at-delimiter-suffix-not-empty-means-xs-not-empty
  (implies (and (true-listp ds)
                (true-listp xs)
                (consp (cadr (split-at-delimiter ds xs))))
           (consp xs)))
(defthm split-at-delimiter-suffix-starts-with-delim-if-possible
  (implies (and (true-listp ds)
                (true-listp xs)
                (consp (cadr (split-at-delimiter ds xs))))
           (member-equal (car (cadr (split-at-delimiter ds xs))) ds)))

(defthm splitoff-match-delivers-shorter-list
  (implies (and (true-listp ps)
                (true-listp xs)
                (consp ps)
                (null (cadr (splitoff-prefix ps xs))))
           (< (len (caddr (splitoff-prefix ps xs)))
              (len xs))))

(defthm splitoff-upr-match-delivers-shorter-list
  (implies (and (true-listp ps)
                (true-listp xs)
                (consp ps)
                (null (cadr (splitoff-prefix-upr ps xs))))
           (< (len (caddr (splitoff-prefix-upr ps xs)))
              (len xs))))

(defthm split-on-token-chr-delivers-shorter-list
  (implies (and (true-listp tok)
                (consp tok)
                (true-listp xs)
                (consp xs))
           (< (len (caddr (split-on-token-chr tok xs)))
              (len xs))))

(defthm split-on-token-delivers-shorter-list
  (implies (and (stringp tok)
                (> (len tok) 0)
                (true-listp xs)
                (consp xs))
           (< (len (caddr (split-on-token tok xs)))
              (len xs))))

