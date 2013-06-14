(add-to-list 'default-frame-alist '(alpha 96 96))

(setq ibuffer-formats '((mark modified read-only " " (name 40 -1) " "
                              (size 6 -1 :right) " " (mode 16 16 :center) " "
                              filename)
                        (mark " " (name 16 -1) " " filename)))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-strip-common-suffix t
      uniquify-separator " *")

(defun set-theme (kind)
  (interactive)
  (load-theme 'whitestone-serious t t)
  (load-theme 'fruitsalad-dark t t)
  (load-theme 'dark-forge t t)
  (cond
   ((eq kind :dark) (progn
                      (disable-theme 'whitestone-serious)
                      ;; (enable-theme 'fruitsalad-dark)
                      (enable-theme 'dark-forge)))
   ((eq kind :light) (progn
                       ;; (disable-theme 'fruitsalad-dark)
                       (disable-theme 'dark-forge)
                       (enable-theme 'whitestone-serious)))))

(require 'calendar)
(defun calendar-cursor-as-kill ()
  (interactive)
  (kill-new (calendar-date-string (calendar-cursor-to-date t))))
(add-hook
 'calendar-mode-hook
 (lambda ()
   (local-set-key (kbd "w") 'calendar-cursor-as-kill)))

(provide 'config-misc)
