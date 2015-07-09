(defun partition-2 (coll)
  (let ((res nil))
    (while coll
      (let ((head (list (first coll) (second coll))))
        (setq coll (cddr coll))
        (setq res (nconc res (list head)))))
    res))
;; (partition-2 (list 1 2 3 4))

(defun re-parse (re s)
  (when (string-match re s)
    (let ((res nil)
          (m (match-data)))
      (while m
        (let ((item (substring-no-properties s (first m) (second m))))
          (setq m (cddr m))
          (setq res (nconc res (list item)))))
      res)))

(defun tokenize-name (s)
  (split-string
   (let ((case-fold-search nil))
     (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s))
   "[_\s/-]+"
   t))

(defun join-aa-bb (s)
  (mapconcat 'downcase (tokenize-name s) "-"))

(defun join-aa.bb (s)
  (mapconcat 'downcase (tokenize-name s) "."))

(defun join-Aa.Bb (s)
  (mapconcat 'capitalize (tokenize-name s) "."))

(defun join-aa_bb (s)
  (mapconcat 'downcase (tokenize-name s) "_"))

(defun join-AA_BB (s)
  (mapconcat 'upcase (tokenize-name s) "_"))

(defun join-aa-space-bb (s)
  (mapconcat 'downcase (tokenize-name s) " "))

(defun join-Aa-space-Bb (s)
  (mapconcat 'capitalize (tokenize-name s) " "))

(defun join-AaBb (s)
  (mapconcat 'capitalize (tokenize-name s) ""))

(defun join-aaBb (s)
  (let ((l (tokenize-name)))
    (concat (downcase (car l))
            (mapconcat 'capitalize (cdr l) ""))))

(defun join-aa/bb (s)
  (mapconcat 'downcase (tokenize-name s) "/"))

(defun join-aa:bb (s)
  (mapconcat 'identity (tokenize-name s) ":"))

(defun join-aa::bb (s)
  (mapconcat 'capitalize (tokenize-name s) "::"))

(defun file-name-relative-to (parent s)
  (let ((user-home-dir (expand-file-name parent)))
    (if (string-prefix-p user-home-dir s)
        (substring s (length user-home-dir))
      s)))

(provide 'lib-sexp)
