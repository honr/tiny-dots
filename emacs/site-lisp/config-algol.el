(defalias 'perl-mode 'cperl-mode)

(require 'cc-mode)
;; (defun config-c-mode-hook () (glasses-mode t) (subword-mode t))
;; (add-hook 'c-mode-common-hook 'config-c-mode-hook)
;; (add-hook 'c-mode-common-hook (lambda () (flyspell-prog-mode)))

(require 'go-mode)

(require 'haskell-indentation)
(require 'haskell-decl-scan)
(require 'haskell-doc)
(require 'inf-haskell)
;; (require 'haskell-ghci)
(require 'haskell-mode)
(setq haskell-mode-hook '(turn-on-haskell-indentation
                          turn-on-eldoc-mode
                          turn-on-haskell-doc-mode
                          turn-on-haskell-decl-scan))
(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))

(define-key c++-mode-map (kbd "C-t o") "->")
(define-key c++-mode-map (kbd "C-t i")
  (lambda ()
    (interactive) (insert "()") (backward-char 1)))
(define-key c++-mode-map (kbd "C-t n")
  (lambda ()
    (interactive) (insert (format "{\n\n}")) (backward-char 2) (c-indent-line)))
(define-key c++-mode-map (kbd "C-t '")
  (lambda ()
    (interactive) (insert (format "\"\"")) (backward-char 1)))
(define-key c++-mode-map (kbd "C-t [")
  (lambda ()
    (interactive) (insert (format "[]")) (backward-char 1)))
(define-key c++-mode-map (kbd "C-t ,")
  (lambda ()
    (interactive) (insert (format "<>")) (backward-char 1)))

(when (require 'dart-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode)))

(provide 'config-algol)
