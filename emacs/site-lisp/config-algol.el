(defalias 'perl-mode 'cperl-mode)

(require 'cc-mode)
(defun config-c-mode-hook ()
  ;; (glasses-mode t)
  (subword-mode t))
(add-hook 'c-mode-common-hook 'config-c-mode-hook)

(require 'haskell-indentation)
(require 'haskell-decl-scan)
(require 'haskell-doc)
(require 'inf-haskell)
(require 'haskell-ghci)
(require 'haskell-mode)
(setq haskell-mode-hook '(turn-on-haskell-indentation
                          turn-on-eldoc-mode
                          turn-on-haskell-doc-mode
                          turn-on-haskell-decl-scan))
(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
