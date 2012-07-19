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
  (cond
   ((eq kind :dark) (progn (disable-theme 'whitestone-serious)
                          (enable-theme 'fruitsalad-dark)))
   ((eq kind :light) (progn (disable-theme 'fruitsalad-dark)
                           (enable-theme 'whitestone-serious)))))
