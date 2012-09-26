(autoload 'ibuffer "ibuffer" "List buffers." t)
(setq ibuffer-auto-mode t)

(global-set-key (kbd "C-x f") prefix-arg)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x f f") 'find-file-at-point)
(global-set-key (kbd "C-x f o") 'ff-find-other-file)
(global-set-key (kbd "C-x f w") 'buffer-file-name-as-kill)

(defun buffer-file-name-as-kill ()
  "Kills the current buffer and kills the frame"
  (interactive)
  (kill-new buffer-file-name))

;; (global-set-key [f4] (lambda () (interactive) (eshell t)))
(global-set-key [f5] 'compile)
;; (global-set-key [M-f2] 'browse-url)
;; (global-set-key [M-S-F2] 'browse-url-at-point)
(global-set-key [f7] 'dict)
(global-set-key (kbd "C-x C-d") 'find-dired)
(global-set-key (kbd "C-x d") 'rgrep)

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

(require 'browse-kill-ring)
(defadvice yank-pop (around kill-ring-browse-maybe (arg))
  "If last action was not a yank, run `browse-kill-ring' instead."
  (if (not (eq last-command 'yank))
      (browse-kill-ring)
    ad-do-it))

(ad-activate 'yank-pop)

(when (not (boundp 'xdg-open-program))
  (error "xdg-open-program not defined"))
;; xdg-open-program must be defined.
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

(defun terminal-here ()
  (interactive)
  (if (and (equal 'darwin system-type)
           (equal 'ns window-system))
      (ns-do-applescript
       (concat
        "tell application \"Terminal\"\n"
        "activate\n"
        (if (string-prefix-p "/ssh:" default-directory)
            (let* ((abc (split-string default-directory ":"))
                   (a (car abc))
                   (b (cadr abc))
                   (c (caddr abc)))
              (concat "do script \"ssh '" b "'"
                      " -t "
                      "'"
                      "cd " c "; "
                      (when buffer-file-name
                        (concat
                         "F=" (substring
                               (caddr (split-string buffer-file-name ":"))
                               (length c))
                         " "))
                      "bash"
                      "'\"\n"))
          (concat "do script \"cd '"
                  (expand-file-name default-directory)
                  "'\"\n"))
        "end tell"))
    ;; Under a terminal. For darwin we assume this is under Xquartz.
    (start-process-shell-command
     "external-xterm" nil "xterm" "-bg '#FFFFFF'"
     (if (string-prefix-p "/ssh:" default-directory)
         (let* ((abc (split-string default-directory ":"))
                (a (car abc))
                (b (cadr abc))
                (c (caddr abc)))
           (concat "-e \"ssh '" b "'"
                   " -t "
                   "'"
                   "cd " c "; "
                   (when buffer-file-name
                     (concat
                      "F=" (substring
                            (caddr (split-string buffer-file-name ":"))
                            (length c))
                      " "))
                   "bash"
                   "'\""))
       ""))))

(global-set-key (kbd "S-<f4>") 'terminal-here)

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
