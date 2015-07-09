(setq inhibit-startup-message t)
(setq backup-inhibited t)
(windmove-default-keybindings)
(setq dired-dwim-target t)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(show-paren-mode t)
(when (boundp 'scroll-bar-mode)
  (set-scroll-bar-mode 'right)
  (scroll-bar-mode -1))
(setq scroll-preserve-screen-position t)
(set-language-environment "UTF-8")
;; (setq completion-styles '(partial-completion initials))
;; (partial-completion-mode t)
(let ((default-directory "~/.emacs.d/site-lisp"))
  (normal-top-level-add-to-load-path (list default-directory))
  (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory "~/.nix-profile/share/emacs/site-lisp"))
  (normal-top-level-add-to-load-path (list default-directory))
  (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory "/usr/share/emacs/site-lisp"))
  (normal-top-level-add-to-load-path (list default-directory))
  (normal-top-level-add-subdirs-to-load-path))
(setq Info-additional-directory-list
      (mapcar 'expand-file-name  ; The trailing slash is *IMPORTANT*.
              (list "~/.emacs.d/info/"
                    "~/.nix-profile/share/info/")))

;; OS-dependent configurations:
(cond ((eq system-type 'darwin)    (require 'config-darwin nil t))
      ((eq system-type 'gnu/linux) (require 'config-linux nil t)))

(when (or (daemonp) (eq system-type 'darwin))
  ;; Add a "layer" of protection around ‘C-x C-c’.
  (global-unset-key (kbd "C-x C-c"))
  (global-set-key (kbd "C-x C-c C-x C-x C-c") 'save-buffers-kill-emacs)

  (iswitchb-mode t) (setq iswitchb-max-to-show 10)

  (savehist-mode t)
  (setq history-length 8192)

  (require 'cl)
  (defalias 'λ 'lambda)

  (require 'dired)
  (require 'dired-x)
  (require 'wdired)
  (put 'dired-find-alternate-file 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (setq mail-user-agent 'message-user-agent)

  (line-number-mode t)
  (column-number-mode t)
  (blink-cursor-mode -1)
  (if (boundp 'tool-bar-mode)
      (tool-bar-mode -1))
  ;; (mouse-avoidance-mode 'banish)

  ;; The original format is:
  ;;   (multiple-frames "%b" ("" invocation-name "@" system-name))
  (setq frame-title-format
        ;; %f: filename, %b: buffer name, %m: mode.
        (list "%b (%m)  +Emacs"))

  (auto-insert-mode t)
  (setq auto-insert-directory "~/.emacs.d/insert/")
  (add-to-list 'auto-insert-alist '(clojure-mode . "hello.clj"))

  (setq gdb-many-windows t)
  (setq gdb-show-main t)

  (require 'subword)
  (global-subword-mode t)

  (require 'newcomment)
  (require 'org)

  (custom-set-variables
   '(compilation-skip-threshold 2)
   ;; '(custom-enabled-themes '(whitestone-serious))
   ;; '(custom-enabled-themes '(fruitsalad-dark))
   '(custom-enabled-themes '(dark-forge))
   '(custom-safe-themes t)
   '(custom-theme-directory "~/.emacs.d/themes")
   '(ediff-window-setup-function 'ediff-setup-windows-plain)
   '(fill-column 78)
   '(flyspell-use-meta-tab nil)
   '(glasses-original-separator "")
   '(glasses-separator "-")
   '(glasses-uncapitalize-p t)
   '(glasses-uncapitalize-regexp "[a-zA-Z_]")
   '(indent-tabs-mode nil)
   ;; '(indicate-buffer-boundaries 'left)
   '(iswitchb-default-method 'maybe-frame)
   ;; '(mouse-avoidance-mode 'banish)
   '(js-indent-level 2)
   '(mouse-wheel-progressive-speed nil)
   '(mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control))))
   '(next-screen-context-lines 8)
   '(org-agenda-files '("~/agenda.org"))
   '(org-export-html-style
     "<link rel=\"stylesheet\" href=\"style.css\">")
   '(org-replace-disputed-keys t)
   '(org-todo-keywords '("TODO" "CANCELLED" "PROGRESSING" "DONE"))
   '(python-guess-indent nil)
   '(read-buffer-completion-ignore-case t)
   '(read-file-name-completion-ignore-case t)
   '(standard-indent 2)
   '(tab-width 2)
   '(vc-follow-symlinks t)
   '(view-read-only t))

  (add-hook 'prog-mode-hook
            (lambda ()
              (setq show-trailing-whitespace t)))

  (defun go-easy-on-remote ()
    (interactive)
    (remove-hook 'find-file-hooks 'vc-find-file-hook)
    (setq vc-handled-backends nil)
    (setq auto-save-interval 0))

  (require 'config-custom-variables nil t)

  (require 'config-gnus nil t)
  (require 'config-utils nil t)
  (require 'config-shell nil t)
  (require 'config-keys nil t)
  (require 'config-misc nil t)
  (require 'config-sexp nil t)
  (require 'config-lisp nil t)
  (require 'config-tex nil t)
  (require 'config-algol nil t)
  ;; Site-specific configurations.
  (require 'config-site nil t)
  ;; Host-specific configurations.
  (require 'config-host nil t))
