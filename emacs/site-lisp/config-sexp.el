(require 'paredit)

(defface paren-face
  '((((class color) (background dark)) (:foreground "#648864"))
    (((class color) (background light)) (:foreground "grey60")))
  "Face used to dim parentheses.")

(dolist (mode (list 'lisp-interaction-mode 'emacs-lisp-mode 'lisp-mode))
  (font-lock-add-keywords mode '(("(\\|)" . 'paren-face)) 'append))

(dolist (hook (list 'emacs-lisp-mode-hook 'lisp-mode-hook))
  (add-hook hook (lambda () (paredit-mode +1))))

(require 'clojure-mode)
(add-hook 'clojure-mode-hook
          (lambda ()
            (paredit-mode +1)
            (subword-mode t)))
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

(setq nxml-sexp-element-flag t)
