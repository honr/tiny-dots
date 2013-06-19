;;; fruitsalad-dark-theme.el --- custom theme for faces

(deftheme fruitsalad-dark
  "A color on black theme, resembling fruitsalad in a dark bowl.")

(custom-theme-set-faces
 'fruitsalad-dark
 '(default                       ((t (:foreground "#EEEEEE" :background "#222222"))))
 '(cursor                        ((t (                      :background "#88FF44"))))
 '(font-lock-builtin-face        ((t (:foreground "#FF88CC"))))
 '(font-lock-comment-face        ((t (:foreground "#777777" :italic t))))
 '(font-lock-constant-face       ((t (:foreground "#00CCCC"))))
 '(font-lock-doc-face            ((t (:foreground "#77AAAA" :background "#333333"))))
 '(font-lock-function-name-face  ((t (:foreground "#FFFFFF" :weight bold))))
 '(font-lock-keyword-face        ((t (:foreground "#88CCFF"))))
 '(font-lock-preprocessor-face   ((t (:foreground "#88CCFF"))))
 '(font-lock-string-face         ((t (:foreground "#00AAAA" :background "#183333"))))
 '(font-lock-type-face           ((t (:foreground "#99DD44"))))
 '(font-lock-variable-name-face  ((t (:foreground "#FFCC88" :weight bold))))
 '(font-lock-warning-face        ((t (:foreground "#FFC0CB" :weight bold))))

 '(dired-directory               ((t (:foreground "#44AAFF"))))
 '(dired-flagged                 ((t (:foreground "#FFFF00" :background "#770000" :weight bold))))
 '(dired-ignored                 ((t (:foreground "#777777"))))
 '(dired-marked                  ((t (:foreground "#008800" :weight bold))))
 '(dired-perm-write              ((t (:foreground "#FF3333" :weight bold))))
 '(dired-symlink                 ((t (:foreground "#FF88FF"))))
 '(ediff-even-diff-A             ((t (                      :background "#181818"))))
 '(ediff-even-diff-B             ((t (                      :background "#181818"))))
 '(ediff-odd-diff-A              ((t (                      :background "#101010"))))
 '(ediff-odd-diff-B              ((t (                      :background "#101010"))))
 '(ediff-current-diff-A          ((t (                      :background "#100000"))))
 '(ediff-current-diff-B          ((t (                      :background "#001000"))))
 '(ediff-fine-diff-A             ((t (                      :background "#300000"))))
 '(ediff-fine-diff-B             ((t (                      :background "#003000"))))
 '(fringe                        ((t (:foreground "#BBBBBB" :background "#222222"))))
 '(header-line                   ((t (:foreground "#888888" :background "#DDDDDD"))))
 '(highlight                     ((t (:foreground "#ADD8E6" :background "#DDDDDD"))))
 '(holiday-face                  ((t (:foreground "#777777" :background "#000000"))))
 '(isearch                       ((t (:foreground "#FFFFFF" :background "#FFBB00"))))
 '(isearch-lazy-highlight-face   ((t (:foreground "#FFFFFF" :background "#FF99BB"))))
 '(menu                          ((t (:foreground "#888888" :background "#000000"))))
 '(minibuffer-prompt             ((t (:foreground "#99FF99"))))
 '(mode-line                     ((t (:foreground "#999999" :background "#444444" :box (:line-width 1 :color "#555555")))))
 '(mode-line-inactive            ((t (:foreground "#666666" :background "#2A2A2A" :box (:line-width 1 :color "#555555")))))
 '(mode-line-buffer-id           ((t (:foreground "#FFFF00"                       :weight bold))))
 '(mode-line-mousable            ((t (:foreground "#444444" :background "#000000"))))
 '(mode-line-mousable-minor-mode ((t (:foreground "#888888" :background "#FFFFFF"))))
 '(paren-face                    ((t (:foreground "#666666"))))
 '(region                        ((t (                      :background "#000000"))))
 '(scroll-bar                    ((t (:foreground "#888888" :background "#222222"))))
 '(secondary-selection           ((t (:foreground "#0088FF" :background "#AACCFF"))))
 '(show-paren-match-face         ((t (:foreground "#FFFFFF" :background "#005500"))))
 '(show-paren-mismatch-face      ((t (:foreground "#FFFFFF" :background "#FF0000"))))
 '(tool-bar                      ((t (:foreground "#777777" :background "#111111"))))
 '(tooltip                       ((t (:foreground "#777777" :background "#333333"))))
 '(widget-button-face            ((t (:foreground "#888888" :weight bold))))
 '(widget-field-face             ((t (:foreground "#999999" :weight bold)))))

(provide-theme 'fruitsalad-dark)
