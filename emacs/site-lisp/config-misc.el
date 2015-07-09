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

(defun compilation-exit-bury-on-success (status code msg)
  (when (and (eq status 'exit) (zerop code))
    (bury-buffer "*compilation*")
    (replace-buffer-in-windows "*compilation*"))
  (cons msg code))
(setq compilation-exit-message-function 'compilation-exit-bury-on-success)


;; TODO: find a better way to let tramp restart a connection.
(require 'tramp)
(defun tramp-try-to-start-possibly-stall-remote (remote-path
                                                 buffer-name-of-tramp)
  (let ((buffer-of-tramp (get-buffer buffer-name-of-tramp)))
    (when (bufferp buffer-of-tramp)
      (kill-buffer buffer-of-tramp)))
  (sit-for .5)
  (find-file-noselect remote-path))
;; Let ssh decide on its own.
(setq tramp-ssh-controlmaster-options "")

;; yasnippet
(require 'yasnippet nil t)
(eval-after-load "yasnippet" ;; (boundp 'yas-global-mode)
  '(progn
     (setq yas-prompt-functions '(yas-completing-prompt)
           yas-also-auto-indent-first-line t
           yas-snippet-dirs (file-expand-wildcards
                             (expand-file-name "~/.emacs.d/snippets/*")))
     (yas-global-mode 1)
     (global-set-key (kbd "C-x r '") 'yas-insert-snippet)
     (global-set-key (kbd "C-x r C-'") 'yas-new-snippet)
     (global-set-key (kbd "C-x r \"") 'yas-visit-snippet-file)))

;; Not sure in what situation these might become necessary:
;; (yas-reload-all)
;; (yas-recompile-all)  ;; Generates a "compiled" snippet file.

(provide 'config-misc)
