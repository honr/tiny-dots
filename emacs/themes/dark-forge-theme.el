;;; dark-forge-theme.el --- custom theme for faces

(deftheme dark-forge
  "A color on black theme.")

(custom-theme-set-faces
 'dark-forge
 '(default ((t (:background "#000000" :foreground "#BBBBBB"))))
 '(cursor ((t (:background "#0088FF"))))

 '(font-lock-builtin-face ((t (:foreground "#F33"))))
 '(font-lock-comment-face ((t (:italic t :foreground "#555"))))
 '(font-lock-constant-face ((t (:foreground "#F33"))))
 '(font-lock-doc-face ((t (:foreground "#F33" :background "#1D1D1D"))))
 '(font-lock-function-name-face ((t (:bold t :foreground "#FFFFFF"))))
 '(font-lock-keyword-face ((t (:foreground "#E80"))))
 '(font-lock-preprocessor-face ((t (:foreground "#E80"))))
 '(font-lock-string-face ((t (:foreground "#888888" :background "#1D1D1D"))))
 '(font-lock-type-face ((t (:foreground "#F33"))))
 '(font-lock-variable-name-face ((t (:bold t :foreground "#FF8800"))))
 '(font-lock-warning-face ((t (:bold t :foreground "Pink"))))

 '(dired-directory ((t (:foreground "#09F"))))
 '(dired-flagged ((t (:foreground "#FF0" :background "#700" :bold t))))
 '(dired-ignored ((t (:foreground "#555"))))
 '(dired-marked ((t (:bold t :foreground "#080"))))
 '(dired-perm-write ((t (:bold t :foreground "#F33"))))
 '(dired-symlink ((t (:foreground "#A0F"))))
 '(fringe ((t (:background "#000000" :foreground "#BBBBBB"))))
 '(header-line ((t (:background "#DDDDDD" :foreground "#888"))))
 '(highlight ((t (:background "#DDDDDD" :foreground "light blue"))))
 '(holiday-face ((t (:background "#000" :foreground "#777"))))
 '(isearch ((t (:background "#FF0000" :foreground "#000000"))))
 '(isearch-lazy-highlight-face ((t (:background "#AA4400" :foreground "#FFFFFF"))))
 '(italic ((t (:bold t))))
 '(menu ((t (:background "#000" :foreground "#888"))))
 '(minibuffer-prompt ((t (:foreground "#F33"))))
 '(mode-line-inactive ((t (:inherit modeline :background "#111111" :foreground "#444"))))
 '(modeline ((t (:background "#282828" :foreground "#999" :box (:line-width 1 :color "#333")))))
 '(modeline-buffer-id ((t (:inherit modeline :foreground "#F80" :weight bold))))
 '(modeline-mousable ((t (:background "#000" :foreground "#444"))))
 '(modeline-mousable-minor-mode ((t (:background "#FFFFFF" :foreground "#888"))))
 '(paren-face ((t (:foreground "#444444"))))
 '(region ((t (:background "#003366"))))
 '(scroll-bar ((t (:background "#000000" :foreground "#444444"))))
 '(secondary-selection ((t (:background "#AACCFF" :foreground "#0088FF"))))
 '(show-paren-match-face ((t (:background "#004400" :foreground "#00FF00"))))
 '(show-paren-mismatch-face ((t (:background "Red" :foreground "White"))))
 '(tool-bar ((t (:background "#111" :foreground "#777"))))
 '(tooltip ((t (:background "#333" :foreground "#777"))))
 '(variable-pitch ((t (nil))))
 '(widget-button-face ((t (:bold t :foreground "#888"))))
 '(widget-field-face ((t (:bold t :foreground "#999")))))

(provide-theme 'dark-forge)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; dark-forge.el ends here
