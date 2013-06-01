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
(let ((default-directory "/usr/share/emacs/site-lisp"))
  (normal-top-level-add-to-load-path (list default-directory))
  (normal-top-level-add-subdirs-to-load-path))
(setq Info-additional-directory-list
      (mapcar 'expand-file-name  ; The trailing slash is *IMPORTANT*.
              (list "~/.emacs.d/info/")))

(defun load-library-if-exists (lib-name)
  (when (locate-file lib-name load-path (get-load-suffixes))
    (load-library lib-name)))

;; OS-dependent configurations:
(cond ((eq system-type 'darwin)    (load-library-if-exists "config-darwin"))
      ((eq system-type 'gnu/linux) (load-library-if-exists "config-linux")))

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
   ;; '(custom-enabled-themes '(whitestone-serious))
   '(custom-enabled-themes '(fruitsalad-dark))
   ;; '(custom-enabled-themes '(dark-forge))
   '(custom-safe-themes t)
   '(custom-theme-directory "~/.emacs.d/themes")
   '(ediff-window-setup-function 'ediff-setup-windows-plain)
   '(fill-column 78)
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
   '(org-agenda-files '("~/Documents/agenda.org"))
   '(org-export-html-style
     (format "<link rel=\"stylesheet\" href=\"%s\">"
             (expand-file-name "~/org.css")))
   '(org-replace-disputed-keys t)
   '(read-buffer-completion-ignore-case t)
   '(read-file-name-completion-ignore-case t)
   '(tab-width 2)
   '(vc-follow-symlinks t))

  (add-hook 'prog-mode-hook
            (lambda ()
              (setq show-trailing-whitespace t)))

  (defun go-easy-on-remote ()
    (interactive)
    (remove-hook 'find-file-hooks 'vc-find-file-hook)
    (setq vc-handled-backends nil)
    (setq auto-save-interval 0))

  (load-library-if-exists "config-custom-variables")

  (load-library-if-exists "config-gnus")
  (load-library-if-exists "config-utils")
  (load-library-if-exists "config-shell")
  (load-library-if-exists "config-keys")
  (load-library-if-exists "config-misc")
  (load-library-if-exists "config-sexp")
  (load-library-if-exists "config-lisp")
  (load-library-if-exists "config-tex")
  (load-library-if-exists "config-algol")
  ;; Site-specific configurations.
  (load-library-if-exists "config-site")
  ;; Host-specific configurations.
  (load-library-if-exists "config-host"))
