(defalias 'perl-mode 'cperl-mode)

(require 'cc-mode)
(defun config-c-mode-hook ()
  ;; (glasses-mode t)
  (subword-mode t))
(add-hook 'c-mode-common-hook 'config-c-mode-hook)

(require 'haskell-mode)
(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
