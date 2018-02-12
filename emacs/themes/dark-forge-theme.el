;;; dark-forge-theme.el --- custom theme for faces

(deftheme dark-forge
  "A color on black theme.")

(custom-theme-set-faces
 'dark-forge
 '(default                       ((t (:foreground "#BBBBBB" :background "#000000"))))
 '(cursor                        ((t (                      :background "#0088FF"))))
 '(font-lock-builtin-face        ((t (:foreground "#FF3333"))))
 '(font-lock-comment-face        ((t (:foreground "#555555" :italic t ))))
 '(font-lock-constant-face       ((t (:foreground "#FF3333"))))
 '(font-lock-doc-face            ((t (:foreground "#FF3333" :background "#1D1D1D"))))
 '(font-lock-function-name-face  ((t (:foreground "#FFFFFF" :weight bold))))
 '(font-lock-keyword-face        ((t (:foreground "#EE8800"))))
 '(font-lock-preprocessor-face   ((t (:foreground "#EE8800"))))
 '(font-lock-string-face         ((t (:foreground "#888888" :background "#1D1D1D"))))
 '(font-lock-type-face           ((t (:foreground "#FF3333"))))
 '(font-lock-variable-name-face  ((t (:foreground "#FF8800" :weight bold))))
 '(font-lock-warning-face        ((t (:foreground "#FF4466" :weight bold))))

 '(dired-directory               ((t (:foreground "#0099FF"))))
 '(dired-flagged                 ((t (:foreground "#FFFF00" :background "#770000" :weight bold))))
 '(dired-ignored                 ((t (:foreground "#555555"))))
 '(dired-marked                  ((t (:foreground "#008800" :weight bold))))
 '(dired-perm-write              ((t (:foreground "#FF3333" :weight bold))))
 '(dired-symlink                 ((t (:foreground "#AA00FF"))))
 '(ediff-even-diff-A             ((t (                      :background "#181818"))))
 '(ediff-even-diff-B             ((t (                      :background "#181818"))))
 '(ediff-odd-diff-A              ((t (                      :background "#101010"))))
 '(ediff-odd-diff-B              ((t (                      :background "#101010"))))
 '(ediff-current-diff-A          ((t (                      :background "#300000"))))
 '(ediff-current-diff-B          ((t (                      :background "#003000"))))
 '(ediff-fine-diff-A             ((t (                      :background "#500000"))))
 '(ediff-fine-diff-B             ((t (                      :background "#005000"))))
 '(fringe                        ((t (:foreground "#BBBBBB" :background "#000000"))))
 '(header-line                   ((t (:foreground "#888888" :background "#181818"))))
 '(highlight                     ((t (                      :background "#003366"))))
 '(holiday-face                  ((t (:foreground "#777777" :background "#000000"))))
 '(isearch                       ((t (:foreground "#FFFF88" :background "#AA0000" :weight bold))))
 '(isearch-lazy-highlight-face   ((t (:foreground "#BB88DD" :background "#330066"))))
 '(menu                          ((t (:foreground "#888888" :background "#000000"))))
 '(minibuffer-prompt             ((t (:foreground "#FF3333"))))
 '(mode-line                     ((t (:foreground "#999999" :background "#282828" :box (:line-width 1 :color "#404040")))))
 '(mode-line-inactive            ((t (:foreground "#444444" :background "#111111" :box (:line-width 1 :color "#202020")))))
 '(mode-line-buffer-id           ((t (:foreground "#FF8800"                       :weight bold))))
 '(mode-line-mousable            ((t (:foreground "#444444" :background "#000000"))))
 '(mode-line-mousable-minor-mode ((t (:foreground "#888888" :background "#FFFFFF"))))
 '(paren-face                    ((t (:foreground "#444444"))))
 '(region                        ((t (                      :background "#003366"))))
 '(scroll-bar                    ((t (:foreground "#FF0000" :background "#010101"))))
 '(secondary-selection           ((t (:foreground "#0088FF" :background "#AACCFF"))))
 '(show-paren-match-face         ((t (:foreground "#00FF00" :background "#004400"))))
 '(show-paren-mismatch-face      ((t (:foreground "#FFFFFF" :background "#FF0000"))))
 '(tool-bar                      ((t (:foreground "#777777" :background "#111111"))))
 '(tooltip                       ((t (:foreground "#777777" :background "#333333"))))

 '(vertical-border               ((t (:foreground "#444444" :background "#777777"))))
 '(widget-button-face            ((t (:foreground "#888888" :weight bold))))
 '(widget-field-face             ((t (:foreground "#999999" :weight bold)))))

(provide-theme 'dark-forge)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; dark-forge.el ends here
