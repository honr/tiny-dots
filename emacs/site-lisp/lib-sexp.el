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
        (let ((beg (first m))
              (end (second m)))
          (setq m (cddr m))
          (setq res (nconc res (list
                                (if (and beg end)
                                    (substring-no-properties s beg end)
                                  nil))))))
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
  (let ((l (tokenize-name s)))
    (concat (downcase (car l))
            (mapconcat 'capitalize (cdr l) ""))))

(defun join-aa/bb (s)
  (mapconcat 'downcase (tokenize-name s) "/"))

(defun join-aa:bb (s)
  (mapconcat 'identity (tokenize-name s) ":"))

(defun join-aa::bb (s)
  (mapconcat 'capitalize (tokenize-name s) "::"))

(defmacro inserter-command (s)
  `(lambda () (interactive) (insert ,s)))

(defmacro joiner-command (joiner)
  `(lambda ()
     (interactive)
     (let ((bounds (bounds-of-thing-at-point 'symbol)))
       (when bounds
         (insert (funcall ,joiner
                          (delete-and-extract-region
                           (car bounds) (cdr bounds))))))))

(defun file-name-relative-to (parent s)
  (let ((user-home-dir (expand-file-name parent)))
    (if (string-prefix-p user-home-dir s)
        (substring s (length user-home-dir))
      s)))

(defun guess-mode-for-file (filename)
  (assoc-default filename auto-mode-alist 'string-match))

(defun mapfmt (fmt-string coll)
  (mapcar (lambda (x) (format fmt-string x)) coll))

;; Example: (global-set-key (kbd "C-c f") (find-file-dir-command "/usr"))
(defmacro find-file-dir-command (dir)
  `(lambda ()
     (interactive)
     (let ((default-directory ,dir))
       (call-interactively 'find-file))))

(provide 'lib-sexp)
