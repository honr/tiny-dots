;; Uncomment to start daemon when running under NextStep.
;; (when (eq window-system 'ns) (server-start))

(defvar xdg-open-program "open")
(setq texmf-root (expand-file-name "~/Library/texmf"))

(cond ((eq window-system 'x)
       (set-face-attribute 'default nil :family "Ubuntu Mono" :height 120))

      ((or (eq window-system 'ns) (eq window-system 'nextstep))
       (progn
         (set-face-attribute 'default nil :family "PT Mono" :height 120)
         (add-to-list 'default-frame-alist '(alpha 90 90))
         (require 'ns-framecontrol)
         (menu-bar-mode -1)
         (setq dired-use-ls-dired nil))))

(provide 'config-darwin)
