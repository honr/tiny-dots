(defalias 'perl-mode 'cperl-mode)

(require 'cc-mode)

(c-add-style "java"
             '((c-basic-offset . 2)
               (c-comment-only-line-offset 0 . 0)
               (c-offsets-alist
                (access-label . 0)
                (arglist-close . c-lineup-arglist)
                (arglist-intro . 4)
                (func-decl-cont . c-lineup-java-throws)
                (inher-cont . c-lineup-java-inher)
                (inline-open . 0)
                (knr-argdecl-intro . 5)
                (label . +)
                (statement-block-intro . +)
                (statement-case-open . +)
                (statement-cont c-lineup-assignments ++)
                (substatement-label . +)
                (substatement-open . +)
                (topmost-intro-cont . +))))

;; Google C/C++ Programming Style.
;; From github.com/google/styleguide/google-c-style.el.  Further modified.
(c-add-style "Google"

             `((c-recognize-knr-p . nil)
               (c-basic-offset . 2)
               (indent-tabs-mode . nil)
               (c-comment-only-line-offset . 0)
               (c-hanging-braces-alist . ((defun-open after)
                                          (defun-close before after)
                                          (class-open after)
                                          (class-close before after)
                                          (inexpr-class-open after)
                                          (inexpr-class-close before)
                                          (namespace-open after)
                                          (inline-open after)
                                          (inline-close before after)
                                          (block-open after)
                                          (block-close . c-snug-do-while)
                                          (extern-lang-open after)
                                          (extern-lang-close after)
                                          (statement-case-open after)
                                          (substatement-open after)))
               (c-hanging-colons-alist . ((case-label)
                                          (label after)
                                          (access-label after)
                                          (member-init-intro before)
                                          (inher-intro)))
               (c-hanging-semi&comma-criteria
                . (c-semi&comma-no-newlines-for-oneline-inliners
                   c-semi&comma-inside-parenlist
                   c-semi&comma-no-newlines-before-nonblanks))
               (c-indent-comments-syntactically-p . t)
               (comment-column . 40)
               (c-indent-comment-alist . ((other . (space . 2))))
               (c-cleanup-list . (brace-else-brace
                                  brace-elseif-brace
                                  brace-catch-brace
                                  empty-defun-braces
                                  defun-close-semi
                                  list-close-comma
                                  scope-operator))
               (c-offsets-alist . ((arglist-intro . 4)
                                   (func-decl-cont . ++)
                                   (member-init-intro . ++)
                                   (inher-intro . ++)
                                   (comment-intro . 0)
                                   (arglist-close . c-lineup-arglist)
                                   (topmost-intro . 0)
                                   (block-open . 0)
                                   (inline-open . 0)
                                   (substatement-open . 0)
                                   (statement-cont c-lineup-assignments ++)
                                   (label . /)
                                   (case-label . +)
                                   (statement-case-open . +)
                                   (statement-case-intro . +) ; case w/o {
                                   (access-label . /)
                                   (innamespace . 0)
                                   (inextern-lang . 0)))))

;; (defun config-c-mode-hook () (glasses-mode t) (subword-mode t))
;; (add-hook 'c-mode-common-hook 'config-c-mode-hook)
;; (add-hook 'c-mode-common-hook (lambda () (flyspell-prog-mode)))

(when (require 'go-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

(when (require 'haskell-mode nil t)
  (require 'haskell-indentation)
  (require 'haskell-decl-scan)
  (require 'haskell-doc)
  (require 'inf-haskell)
  (require 'haskell-ghci nil t)
  (setq haskell-mode-hook '(turn-on-haskell-indentation
                            turn-on-eldoc-mode
                            turn-on-haskell-doc-mode
                            turn-on-haskell-decl-scan))
  (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode)))

(when (require 'protobuf-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode)))

(add-to-list 'auto-mode-alist '("/BUILD$" . python-mode))

(when (require 'dart-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode)))

(when (require 'lua-mode nil t)
  (setq lua-indent-level 2)
  (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode)))

(when (require 'javascript-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.javascript\\'" . javascript-mode)))

(define-key c++-mode-map (kbd "; a n")
  (lambda ()
    (interactive)
    (insert (format "{\n\n}")) (backward-char 2) (c-indent-line)))
(define-key c++-mode-map (kbd "; a ,")
  (lambda ()
    (interactive)
    (insert (format "<>")) (backward-char 1)))

(provide 'config-algol)
