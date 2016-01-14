(defun add-to-load-path-if-exists (dir)
  (when (file-exists-p dir)
    (let ((default-directory dir))
      (normal-top-level-add-to-load-path (list default-directory))
      (normal-top-level-add-subdirs-to-load-path))))
(add-to-load-path-if-exists "~/.emacs.d/site-lisp")
(add-to-load-path-if-exists "~/.nix-profile/share/emacs/site-lisp")
(add-to-load-path-if-exists "/usr/share/emacs/site-lisp")
(setq Info-additional-directory-list
      (mapcar 'expand-file-name  ; The trailing slash is *IMPORTANT*.
              (list "~/.emacs.d/info/"
                    "~/.nix-profile/share/info/")))

;; OS-dependent configurations:
(cond ((eq system-type 'darwin) (require 'config-darwin nil t))
      ((eq system-type 'gnu/linux) (require 'config-linux nil t)))

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

;; Wrap the rest of the file in a condition such as the following, if a light
;; version of emacs is sometimes desired.
;; (when (or (daemonp) (and (boundp 'server-running-p) (server-running-p))) ...)

;; Add a "layer" of protection around ‘C-x C-c’.
(global-unset-key (kbd "C-x C-c"))
(global-set-key (kbd "C-x C-c C-x C-x C-c") 'save-buffers-kill-emacs)
;; (icomplete-mode t)  ;; Diabled because it breaks several things...
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
(setq ring-bell-function 'ignore)
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

(setq gdb-many-windows t)
(setq gdb-show-main t)

(require 'subword)
(global-subword-mode t)

(require 'newcomment)
(require 'org)

(setq-default abbrev-mode t)

(custom-set-variables
 '(compilation-skip-threshold 2)
 '(css-indent-offset 2)
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
 '(gud-gdb-command-name "gdb --annotate=1")
 ;; '(icomplete-prospects-height 1)
 ;; '(icomplete-show-matches-on-no-input t)
 '(indent-tabs-mode nil)
 ;; currently disabled until theming is in place
 ;; '(indicate-buffer-boundaries 'left)
 '(js-indent-level 2)
 '(large-file-warning-threshold nil)
 '(major-mode 'org-mode)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control))))
 '(next-screen-context-lines 8)
 '(org-agenda-files (quote ("~/org/a.org" "~/org/current.org")))
 '(org-default-notes-file "~/org/notes.org")
 '(org-html-head-extra "<script src=\"prettify/run_prettify.js\"></script>")
 '(org-html-head-include-scripts nil)
 '(org-html-postamble t)
 '(org-html-postamble-format
   '(("en" "<span class=\"author\">author: %a (%e)</span>,
<span class=\"date\">mtime: %C</span>")))
 '(org-html-preamble nil)
 '(org-html-style-default "<link rel=\"stylesheet\" href=\"style.css\"/>")
 '(org-replace-disputed-keys t)
 '(org-todo-keywords '("TODO" "CANCELLED" "PROGRESSING" "DONE"))
 '(package-selected-packages '(powerline))
 '(python-guess-indent nil)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(standard-indent 2)
 '(tab-width 2)
 '(vc-follow-symlinks t)
 '(view-read-only t))

(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

(package-initialize)
(require 'config-custom-variables nil t)
(require 'config-gnus nil t)
(require 'config-utils nil t)
(require 'config-shell nil t)
(require 'config-sexp nil t)
(require 'config-algol nil t)
(require 'config-keys nil t)
(require 'config-misc nil t)
(require 'config-lisp nil t)
(require 'config-tex nil t)
(require 'config-site nil t)          ; Site-specific configurations.
(require 'config-host nil t)          ; Host-specific configurations.
