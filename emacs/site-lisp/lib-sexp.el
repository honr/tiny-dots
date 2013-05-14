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

(provide 'lib-sexp)
