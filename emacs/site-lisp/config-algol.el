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
(require 'haskell-ghci)
(require 'haskell-mode)
(setq haskell-mode-hook '(turn-on-haskell-indentation
                          turn-on-eldoc-mode
                          turn-on-haskell-doc-mode
                          turn-on-haskell-decl-scan))
(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))

(define-key c-mode-map (kbd "S-SPC") "_")
(define-key c++-mode-map (kbd "S-SPC") "_")

(define-key c++-mode-map (kbd "C-t") prefix-arg)
(define-key c++-mode-map (kbd "C-t j")
  (lambda ()
    (interactive)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (insert (join-AaBb
                 (delete-and-extract-region
                  (car bounds) (cdr bounds))))))))

(define-key c++-mode-map (kbd "C-t k")
  (lambda ()
    (interactive)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (insert (join-AA_BB
                 (delete-and-extract-region
                  (car bounds) (cdr bounds))))))))

(define-key c++-mode-map (kbd "C-t l")
  (lambda ()
    (interactive)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (insert (join-aa-bb
                 (delete-and-extract-region
                  (car bounds) (cdr bounds))))))))

(provide 'config-algol)
