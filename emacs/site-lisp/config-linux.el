(setq x-select-enable-clipboard t)
(defvar xdg-open-program "xdg-open")

(add-to-list 'default-frame-alist '(scroll-bar-width . 11))

;; Redefine this funcion.
(defun browse-url-can-use-xdg-open () t)

(provide 'config-linux)
