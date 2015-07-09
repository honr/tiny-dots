(cond ((eq window-system 'x)
       (set-face-attribute 'default nil :family "Ubuntu Mono" :height 120))

      ((or (eq window-system 'ns) (eq window-system 'nextstep))
       (progn
         (set-face-attribute 'default nil :family "Ubuntu Mono" :height 140)
         (add-to-list 'default-frame-alist '(alpha 90 90))
         (menu-bar-mode -1))))

(require 'framecontrol-mac)

(provide 'config-darwin)
