(defun mutate-region (f)
  (interactive "aFunction to apply to region: ")
  (save-excursion
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (let ((replacement-text (funcall f (buffer-string))))
        (delete-region (point-min) (point-max))
        (insert replacement-text)))))

(defun tabulize-with-percentage (input-string)
  "Make an org table out of something like \"foo 0.4\\nbar 0.6\""
  (let* ((raw-table (split-string input-string "\n" t))
         (table (mapcar (lambda (kv-str)
                          (let* ((kv (split-string kv-str))
                                 (k (first kv))
                                 (v (string-to-number (second kv))))
                            (list k v)))
                        raw-table))
         (values (mapcar 'second table))
         (sum (apply '+ 0.0 values))
         (max-key-length (apply 'max (length "TOTAL")
                                (mapcar (lambda (kv) (length (first kv)))
                                        table)))
         (fmt-string (format "| %%-%ds | %%8.4f | %%5.1f%%%% |"
                             max-key-length)))
    (concat
     (mapconcat (lambda (kv)
                  (let ((k (first kv))
                        (v (second kv)))
                    (format fmt-string
                            k v (* 100.0 (/ v sum))))) table "\n")
     "\n"
     (format fmt-string "TOTAL" sum 100.0))))
