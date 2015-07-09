(add-to-list 'default-frame-alist '(alpha 96 96))

(setq ibuffer-formats '((mark modified read-only " " (name 40 -1) " "
                              (size 6 -1 :right) " " (mode 16 16 :center) " "
                              filename)
                        (mark " " (name 16 -1) " " filename)))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-strip-common-suffix t
      uniquify-separator " *")

(defun set-theme (new-theme)
  (interactive)

  (dolist (th custom-enabled-themes)
    (disable-theme th))

  (load-theme new-theme))

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
(let ((possible-yas-snippet-dirs
       (file-expand-wildcards
        (expand-file-name "~/.emacs.d/snippets/*"))))
  (when possible-yas-snippet-dirs       ; We have snippets to load.
    (require 'yasnippet nil t)
    (eval-after-load "yasnippet" ;; (boundp 'yas-global-mode)
      '(progn
         (setq yas-prompt-functions '(yas-completing-prompt)
               yas-also-auto-indent-first-line t
               yas-snippet-dirs possible-yas-snippet-dirs)
         (yas-global-mode 1)
         (global-set-key (kbd "C-x r '") 'yas-insert-snippet)
         (global-set-key (kbd "C-x r C-'") 'yas-new-snippet)
         (global-set-key (kbd "C-x r \"") 'yas-visit-snippet-file)))))

;; Not sure in what situation these might become necessary:
;; (yas-reload-all)
;; (yas-recompile-all)  ;; Generates a "compiled" snippet file.

(defun wrap-lines-in-quotes (beg end &optional prefixarg)
  "Wraps lines in quotes while quoting the string itself.  Uses
regular emacs-lisp prin1 for quoting.  With a prefix arg, places
prefix spaces into the quotes."
  (interactive "*r\np")
  (let ((re (if (zerop (logand 1 prefixarg)) "^\\(.*\\)$" "^\s*\\(.*\\)$"))
        (line-prefix (if (zerop (logand 2 prefixarg)) "" "  "))
        (line-trail (if (zerop (logand 4 prefixarg)) "" "\\n")))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (while (search-forward-regexp re)
          (replace-match
           (concat "\"" line-prefix
                   (substring (prin1-to-string
                               (match-string-no-properties 1)) 1 -1)
                   line-trail "\"")
           t t nil 1))))))
(global-set-key (kbd "C-M-\"") 'wrap-lines-in-quotes)

(when (require 'markdown-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode)))

(defun go-easy-on-remote ()
  (interactive)
  (remove-hook 'find-file-hooks 'vc-find-file-hook)
  (setq vc-handled-backends nil)
  (setq auto-save-timeout nil)
  (setq auto-save-interval 0))

(provide 'config-misc)
