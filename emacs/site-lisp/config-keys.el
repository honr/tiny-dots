(require 'lib-sexp)

(autoload 'ibuffer "ibuffer" "List buffers." t)
(setq ibuffer-auto-mode t)

(global-set-key (kbd "C-x f") prefix-arg)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x f f") 'find-file-at-point)
(global-set-key (kbd "C-x f 4 f") 'ffap-other-window)
(global-set-key (kbd "C-x f 5 f") 'ffap-other-frame)
(global-set-key (kbd "C-x f o") 'ff-find-other-file)
(global-set-key (kbd "C-x f r") 'ff-find-related-file)
(global-set-key (kbd "C-x f w") 'buffer-file-name-as-kill)

(defun toggle-window-split-direction ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(define-key ctl-x-4-map "t" 'toggle-window-split-direction)

(defun buffer-file-name-as-kill ()
  "Copy the name of the file name of current buffer to the kill-ring"
  (interactive)
  (kill-new (or buffer-file-name "")))

;; (global-set-key [f4] (lambda () (interactive) (eshell t)))
;; (global-set-key (kbd "C-q") prefix-arg)
;; (global-set-key (kbd "C-q C-a") 'quoted-insert)
;; (global-set-key (kbd "C-q l") 'compile)
;; (global-set-key [M-f2] 'browse-url)
;; (global-set-key [M-S-F2] 'browse-url-at-point)
;; (global-set-key (kbd "C-q d") 'dict)
;; (global-set-key (kbd "C-q f") 'find-dired)
;; (global-set-key (kbd "C-q g") 'rgrep)
;; (global-set-key (kbd "C-x C-d") 'find-dired)
;; (global-set-key (kbd "C-x d") 'rgrep)
;; (global-set-key (kbd "C-S-k") 'comment-or-uncomment-line)
;; (global-set-key [C-S-w] 'comment-region-and-duplicate)

(require 'comint)
(define-key comint-mode-map (kbd "M-p") 'comint-previous-matching-input-from-input)
(define-key comint-mode-map (kbd "M-n") 'comint-next-matching-input-from-input)
(define-key comint-mode-map (kbd "C-c M-p") 'comint-previous-input)
(define-key comint-mode-map (kbd "C-c M-n") 'comint-next-input)

(define-key minibuffer-local-map (kbd "M-p") 'previous-complete-history-element)
(define-key minibuffer-local-map (kbd "M-n") 'next-complete-history-element)
(define-key minibuffer-local-map (kbd "<up>") 'previous-complete-history-element)
(define-key minibuffer-local-map (kbd "<down>") 'next-complete-history-element)

(defun delete-frame-and-buffer ()
  "Kills the current buffer and kills the frame"
  (interactive)
  (kill-buffer (current-buffer)) (delete-frame))
(global-set-key "\C-x5k" 'delete-frame-and-buffer)

(global-set-key (kbd "S-SPC") "_")

(global-set-key (kbd "C-t") prefix-arg)

;; Join multiword symbols
(global-set-key (kbd "C-t k") (joiner-command 'join-AaBb))
(global-set-key (kbd "C-t u") (joiner-command 'join-AA_BB))
(global-set-key (kbd "C-t j") (joiner-command 'join-aa-bb))
(global-set-key (kbd "C-t l") (joiner-command 'join-aa_bb))
(global-set-key (kbd "C-t h") (joiner-command 'join-aaBb))
(global-set-key (kbd "C-t s") (joiner-command 'join-aa-space-bb))
(global-set-key (kbd "C-t s") (joiner-command 'join-Aa-space-Bb))

;; Shift and transpose.
(global-set-key (kbd "C-S-f") (lambda () (interactive) (transpose-chars 1)))
(global-set-key (kbd "C-S-b") (lambda () (interactive) (transpose-chars -1)))
(global-set-key (kbd "M-F") (lambda () (interactive) (transpose-words 1)))
(global-set-key (kbd "M-B") (lambda () (interactive) (transpose-words -1)))
(global-set-key (kbd "M-E") (lambda () (interactive) (transpose-sentences 1)))
(global-set-key (kbd "M-A") (lambda () (interactive) (transpose-sentences -1)))
(global-set-key (kbd "C-S-n") (lambda () (interactive) (transpose-lines 1)))
(global-set-key (kbd "C-S-p") (lambda () (interactive) (transpose-lines -1)))
(global-set-key (kbd "M-C-S-f") (lambda () (interactive) (transpose-sexps 1)))
(global-set-key (kbd "M-C-S-b") (lambda () (interactive) (transpose-sexps -1)))

;; Greek, math, and a few other symbols.
(global-set-key (kbd "M-g a") "α")
(global-set-key (kbd "M-g b") "β")
(global-set-key (kbd "M-g m") "µ")
(global-set-key (kbd "M-g t") "θ")
(global-set-key (kbd "M-g g") "γ")
(global-set-key (kbd "M-g d") "δ")
(global-set-key (kbd "M-g e") "ε")
(global-set-key (kbd "M-g z") "ζ")
(global-set-key (kbd "M-g h") "η")
(global-set-key (kbd "M-g q") "θ")
(global-set-key (kbd "M-g i") "ι")
(global-set-key (kbd "M-g k") "κ")
(global-set-key (kbd "M-g l") "λ")
(global-set-key (kbd "M-g m") "μ")
(global-set-key (kbd "M-g n") "ν")
(global-set-key (kbd "M-g c") "ξ")
(global-set-key (kbd "M-g o") "ο")
(global-set-key (kbd "M-g p") "π")
(global-set-key (kbd "M-g r") "ρ")
;; ς
(global-set-key (kbd "M-g s") "σ")
(global-set-key (kbd "M-g t") "τ")
(global-set-key (kbd "M-g y") "υ")
(global-set-key (kbd "M-g f") "φ")
(global-set-key (kbd "M-g x") "χ")
(global-set-key (kbd "M-g u") "ψ")
(global-set-key (kbd "M-g w") "ω")

(global-set-key (kbd "M-g A") "Α")
(global-set-key (kbd "M-g B") "Β")
(global-set-key (kbd "M-g G") "Γ")
(global-set-key (kbd "M-g D") "Δ")
(global-set-key (kbd "M-g E") "Ε")
(global-set-key (kbd "M-g Z") "Ζ")
(global-set-key (kbd "M-g H") "Η")
(global-set-key (kbd "M-g Q") "Θ")
(global-set-key (kbd "M-g I") "Ι")
(global-set-key (kbd "M-g K") "Κ")
(global-set-key (kbd "M-g L") "Λ")
(global-set-key (kbd "M-g M") "Μ")
(global-set-key (kbd "M-g N") "Ν")
(global-set-key (kbd "M-g C") "Ξ")
(global-set-key (kbd "M-g O") "Ο")
(global-set-key (kbd "M-g P") "Π")
(global-set-key (kbd "M-g R") "Ρ")
;; ΢
(global-set-key (kbd "M-g S") "Σ")
(global-set-key (kbd "M-g T") "Τ")
(global-set-key (kbd "M-g Y") "Υ")
(global-set-key (kbd "M-g F") "Φ")
(global-set-key (kbd "M-g X") "Χ")
(global-set-key (kbd "M-g U") "Ψ")
(global-set-key (kbd "M-g W") "Ω")

(global-set-key (kbd "M-g \"")
                (lambda () (interactive) (insert "“”") (forward-char -1)))
(global-set-key (kbd "M-g '")
                (lambda () (interactive) (insert "‘’") (forward-char -1)))
(global-set-key (kbd "M-g <up>") "↑")
(global-set-key (kbd "M-g <right>") "→")
(global-set-key (kbd "M-g <down>") "↓")
(global-set-key (kbd "M-g <left>") "←")
(global-set-key (kbd "M-g <") "≤")
(global-set-key (kbd "M-g >") "≥")

(global-set-key (kbd "M-g \\") "║")
(global-set-key (kbd "M-g /") (inserter-command "¦"))
(global-set-key (kbd "M-g .") "•")
(global-set-key (kbd "M-g `") (inserter-command "º"))
(global-set-key (kbd "M-g ,") "◊")

(require 'browse-kill-ring)
(defadvice yank-pop (around kill-ring-browse-maybe (arg))
  "If last action was not a yank, run `browse-kill-ring' instead."
  (if (not (eq last-command 'yank))
      (browse-kill-ring)
    ad-do-it))

(ad-activate 'yank-pop)

(when (not (boundp 'xdg-open-program))
  ;; Set a not-so-last resort.
  (setq xdg-open-program (expand-file-name "~/.local/bin/visit"))
  (when (not (file-exists-p xdg-open-program))
    (error "xdg-open-program does not exist.  Please set one.")))

(defun xdg-open-filelist (filelist &optional rest)
  (apply 'call-process
         (or rest
             xdg-open-program) nil 0 nil filelist)
  (message (format "%s %s" rest filelist)))

(defun dired-xdg-open (&optional arg filelist)
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg)))
     (list (when current-prefix-arg
             (dired-read-shell-command
              (concat "! on "
                      "%s: ")
              current-prefix-arg
              files))
           files)))
  (xdg-open-filelist filelist arg))

(define-key dired-mode-map (kbd "C-;") 'dired-xdg-open)
(define-key wdired-mode-map (kbd "C-;") 'dired-xdg-open)

(defun terminal-here--build-remote-command (default-directory buffer-file-name)
  (when (or (string-prefix-p "/ssh:" default-directory)
            (string-prefix-p "/rsync:" default-directory)
            (string-prefix-p "/scp:" default-directory)
            (string-prefix-p "/s:" default-directory))
    (let* ((abc (split-string default-directory ":"))
           (ignored--remote-pseudo-protocol (car abc)) ; -ignored
           (remote-host (cadr abc))                    ; remote
           (remote-path (caddr abc))                   ; path on the remote
           (remote-filename (when buffer-file-name
                              (substring
                               (caddr (split-string buffer-file-name ":"))
                               (length remote-path)))))
      ;; CAVEAT: assumes remote-path and remote-filename are single bash
      ;; tokens, and have no '$' in them.
      (format "ssh '%s' -t 'cd %s; %sbash'"
              remote-host
              remote-path
              (if remote-filename
                  (format "F=%s " remote-filename)
                "")))))

(defun terminal-here ()
  (interactive)
  (if (and (equal 'darwin system-type) (equal 'ns window-system))
      (ns-do-applescript
       (concat
        "tell application \"Terminal\"\n"
        "activate\n"
        (format "do script \"%s\"\n"
                (or (terminal-here--build-remote-command
                     default-directory buffer-file-name)
                    (format "cd '%s'"
                            (expand-file-name default-directory))))
        "end tell"))
    ;; Under a terminal. For darwin we assume this is under Xquartz.
    (start-process-shell-command
     "external-xterm" nil "xterm"
     (format "-bg '%s'" (face-attribute 'default :background))
     (let ((rem (terminal-here--build-remote-command
                 default-directory buffer-file-name)))
       (if rem (format "-e \"%s\"" rem) "")))))

(global-set-key (kbd "S-<f4>") 'terminal-here)
;; (global-set-key (kbd "C-q j") 'terminal-here)

(defun shell-here ()
  "Open a shell in `default-directory'."
  (interactive)
  (let ((dir (expand-file-name default-directory))
        (buf (or (get-buffer "*shell*") (shell))))
    (goto-char (point-max))
    (if (not (string= (buffer-name) "*shell*"))
        (switch-to-buffer-other-window buf))
    (message list-buffers-directory)
    (if (not (string= (expand-file-name list-buffers-directory) dir))
        (progn (comint-send-string (get-buffer-process buf)
                                   (concat "cd '" dir "'\n"))
               (setq list-buffers-directory dir)))))

(global-set-key (kbd "C-c !") 'shell-here)

(defun external-directory-browser-here ()
  (interactive)
  (start-process-shell-command
   "external-nautilus" nil "xdg-open ."))

(global-set-key (kbd "M-<f4>") 'external-directory-browser-here)
;; (global-set-key (kbd "C-q n") 'external-directory-browser-here)

(global-set-key (kbd "C-x f a") 'org-agenda)
(global-set-key (kbd "C-x f c") 'calendar)

(when (require 'magit nil t)
  (global-set-key (kbd "C-x f g") 'magit-status))

(defun define-semicolon-prefix-keys ()
  (interactive)

  ;; Remove a few local bindings that would overshadow the global definition
  ;; of this prefix key (";").
  (dolist (keymap (list paredit-mode-map
                        java-mode-map
                        c++-mode-map
                        c-mode-map
                        c-mode-base-map))
    (define-key keymap (kbd ";") nil))

  (global-set-key (kbd ";") prefix-arg)
  (global-set-key (kbd "; SPC") (inserter-command "; "))
  (global-set-key (kbd "; ;") (inserter-command ";"))
  (global-set-key (kbd "; 2") (inserter-command ";; "))
  (global-set-key (kbd "; 3") (inserter-command ";;; "))
  (global-set-key (kbd "; 4") (inserter-command ";;;; "))
  (global-set-key (kbd "; RET") (lambda () (interactive) (insert ";") (newline)))
  (global-set-key (kbd "; C-j") (lambda ()
                                  (interactive)
                                  (insert ";")
                                  (electric-newline-and-maybe-indent)))
  (global-set-key (kbd "; l") 'execute-extended-command)
  (global-set-key (kbd "; u") 'undo)
  (global-set-key (kbd "; s") 'isearch-forward-regexp)
  (global-set-key (kbd "; r") 'isearch-backward-regexp)
  (global-set-key (kbd "; q") 'query-replace-regexp)
  (global-set-key (kbd "; t") 'terminal-here)
  (global-set-key (kbd "; v") 'switch-to-buffer)
  (global-set-key (kbd "; n") 'switch-to-buffer-other-frame)
  (global-set-key (kbd "; b") 'ibuffer)

  (global-set-key (kbd "; f") prefix-arg)
  (global-set-key (kbd "; f a") 'org-agenda)
  (global-set-key (kbd "; f c") 'calendar)
  (global-set-key (kbd "; f d") 'find-dired)
  (global-set-key (kbd "; f f") 'find-file)
  (global-set-key (kbd "; f g") 'rgrep)
  (global-set-key (kbd "; f j") 'dired-jump)
  (global-set-key (kbd "; f k") 'kill-buffer)
  (global-set-key (kbd "; f n") 'ffap-other-frame)
  (global-set-key (kbd "; f o") 'ff-find-other-file)
  (global-set-key (kbd "; f p") 'find-file-at-point)
  (global-set-key (kbd "; f q") 'delete-frame-and-buffer)
  (global-set-key (kbd "; f r") 'ff-find-related-file)
  (global-set-key (kbd "; f s") 'save-buffer)
  (when (fboundp 'magit-status)
    (global-set-key (kbd "; f t") 'magit-status))
  (global-set-key (kbd "; f v") 'view-mode)
  (global-set-key (kbd "; f w") 'buffer-file-name-as-kill)
  (global-set-key (kbd "; f 3") 'server-edit)
  (global-set-key (kbd "; f 4") 'ffap-other-window)
  (global-set-key (kbd "; f 5") 'ffap-other-frame)

  (global-set-key (kbd "; a") prefix-arg)
  (global-set-key (kbd "; a c") 'browse-url)
  (global-set-key (kbd "; a d") 'dict)
  (global-set-key (kbd "; a g") 'goto-line)

  (global-set-key (kbd "; a h") (joiner-command 'join-aaBb))
  (global-set-key (kbd "; a j") (joiner-command 'join-aa-bb))
  (global-set-key (kbd "; a k") (joiner-command 'join-AaBb))
  (global-set-key (kbd "; a l") (joiner-command 'join-aa_bb))
  (global-set-key (kbd "; a u") (joiner-command 'join-AA_BB))
  (global-set-key (kbd "; a s") (joiner-command 'join-aa-space-bb))
  (global-set-key (kbd "; a n") (joiner-command 'join-Aa-space-Bb))

  (global-set-key (kbd "; w") prefix-arg)
  (global-set-key (kbd "; w w") 'other-window)
  (global-set-key (kbd "; w f") 'windmove-right)
  (global-set-key (kbd "; w b") 'windmove-left)
  (global-set-key (kbd "; w n") 'windmove-down)
  (global-set-key (kbd "; w p") 'windmove-up)
  (global-set-key (kbd "; w k") 'delete-window)
  (global-set-key (kbd "; w j") 'split-window-below)
  (global-set-key (kbd "; w l") 'split-window-right)
  (global-set-key (kbd "; w d") 'make-frame-command)
  (global-set-key (kbd "; w e") 'balance-windows)
  (global-set-key (kbd "; w i") 'delete-other-windows)
  (global-set-key (kbd "; w t") 'toggle-window-split-direction)

  (global-set-key (kbd "; h") prefix-arg)
  (define-key global-map (kbd "; h") 'help-command)

  (progn
    (global-set-key (kbd "; g") prefix-arg)
    (global-set-key (kbd "; g a") "α")
    (global-set-key (kbd "; g b") "β")
    (global-set-key (kbd "; g m") "µ")
    (global-set-key (kbd "; g t") "θ")
    (global-set-key (kbd "; g g") "γ")
    (global-set-key (kbd "; g d") "δ")
    (global-set-key (kbd "; g e") "ε")
    (global-set-key (kbd "; g z") "ζ")
    (global-set-key (kbd "; g h") "η")
    (global-set-key (kbd "; g q") "θ")
    (global-set-key (kbd "; g i") "ι")
    (global-set-key (kbd "; g k") "κ")
    (global-set-key (kbd "; g l") "λ")
    (global-set-key (kbd "; g m") "μ")
    (global-set-key (kbd "; g n") "ν")
    (global-set-key (kbd "; g c") "ξ")
    (global-set-key (kbd "; g o") "ο")
    (global-set-key (kbd "; g p") "π")
    (global-set-key (kbd "; g r") "ρ")
    ;; ς
    (global-set-key (kbd "; g s") "σ")
    (global-set-key (kbd "; g t") "τ")
    (global-set-key (kbd "; g y") "υ")
    (global-set-key (kbd "; g f") "φ")
    (global-set-key (kbd "; g x") "χ")
    (global-set-key (kbd "; g u") "ψ")
    (global-set-key (kbd "; g w") "ω")

    (global-set-key (kbd "; g A") "Α")
    (global-set-key (kbd "; g B") "Β")
    (global-set-key (kbd "; g G") "Γ")
    (global-set-key (kbd "; g D") "Δ")
    (global-set-key (kbd "; g E") "Ε")
    (global-set-key (kbd "; g Z") "Ζ")
    (global-set-key (kbd "; g H") "Η")
    (global-set-key (kbd "; g Q") "Θ")
    (global-set-key (kbd "; g I") "Ι")
    (global-set-key (kbd "; g K") "Κ")
    (global-set-key (kbd "; g L") "Λ")
    (global-set-key (kbd "; g M") "Μ")
    (global-set-key (kbd "; g N") "Ν")
    (global-set-key (kbd "; g C") "Ξ")
    (global-set-key (kbd "; g O") "Ο")
    (global-set-key (kbd "; g P") "Π")
    (global-set-key (kbd "; g R") "Ρ")
    ;; ΢
    (global-set-key (kbd "; g S") "Σ")
    (global-set-key (kbd "; g T") "Τ")
    (global-set-key (kbd "; g Y") "Υ")
    (global-set-key (kbd "; g F") "Φ")
    (global-set-key (kbd "; g X") "Χ")
    (global-set-key (kbd "; g U") "Ψ")
    (global-set-key (kbd "; g W") "Ω")

    (global-set-key (kbd "; g \"")
                    (lambda () (interactive) (insert "“”") (forward-char -1)))
    (global-set-key (kbd "; g '")
                    (lambda () (interactive) (insert "‘’") (forward-char -1)))
    (global-set-key (kbd "; g <up>") "↑")
    (global-set-key (kbd "; g <right>") "→")
    (global-set-key (kbd "; g <down>") "↓")
    (global-set-key (kbd "; g <left>") "←")
    (global-set-key (kbd "; g <") "≤")
    (global-set-key (kbd "; g >") "≥")

    (global-set-key (kbd "; g \\") "║")
    (global-set-key (kbd "; g /") (inserter-command "¦"))
    (global-set-key (kbd "; g .") "•")
    (global-set-key (kbd "; g `") (inserter-command "º"))
    (global-set-key (kbd "; g ,") "◊")
    )
  )
(define-semicolon-prefix-keys)

(provide 'config-keys)
