;;; dark-ruthless-theme.el --- custom theme for faces

(deftheme dark-ruthless
  "A white and gray on black theme.")

(let ((base-fg-color "#FFF"))
 (custom-theme-set-faces
  'dark-ruthless
  `(default ((t (:background "#000000" :foreground "#666666"))))
  `(cursor ((t (:background "#FFFFFF"))))

  `(font-lock-builtin-face ((t (:foreground ,base-fg-color))))
  `(font-lock-comment-face ((t (:italic t :foreground "#333"))))
  `(font-lock-constant-face ((t (:foreground ,base-fg-color))))
  `(font-lock-doc-face ((t (:foreground ,base-fg-color :background "#181818"))))
  `(font-lock-function-name-face ((t (:bold t :foreground "#FFFFFF"))))
  `(font-lock-keyword-face ((t (:foreground ,base-fg-color))))
  `(font-lock-preprocessor-face ((t (:foreground ,base-fg-color))))
  `(font-lock-string-face ((t (:foreground "#888888" :background "#181818"))))
  `(font-lock-type-face ((t (:foreground ,base-fg-color))))
  `(font-lock-variable-name-face ((t (:bold t :foreground "#AAA"))))
  `(font-lock-warning-face ((t (:bold t :foreground ,base-fg-color))))

  `(dired-directory ((t (:foreground "#09F"))))
  `(dired-flagged ((t (:foreground "#FF0" :background "#700" :bold t))))
  `(dired-ignored ((t (:foreground "#555"))))
  `(dired-marked ((t (:bold t :foreground "#080"))))
  `(dired-perm-write ((t (:bold t :foreground ,base-fg-color))))
  `(dired-symlink ((t (:foreground "#A0F"))))
  `(fringe ((t (:background "#000000" :foreground "#BBBBBB"))))
  `(header-line ((t (:background "#DDDDDD" :foreground "#888"))))
  `(highlight ((t (:background "#DDDDDD" :foreground "light blue"))))
  `(holiday-face ((t (:background "#000" :foreground "#777"))))
  `(isearch ((t (:background "#FF0000" :foreground "#000000"))))
  `(isearch-lazy-highlight-face ((t (:background "#AA4400" :foreground "#FFFFFF"))))
  `(italic ((t (:bold t))))
  `(menu ((t (:background "#000" :foreground "#888"))))
  `(minibuffer-prompt ((t (:foreground ,base-fg-color))))
  `(mode-line-inactive ((t (:inherit modeline :background "#111111" :foreground "#444"))))
  `(mode-line ((t (:background "#282828" :foreground "#999" :box (:line-width 1 :color "#333")))))
  `(mode-line-buffer-id ((t (:inherit modeline :foreground ,base-fg-color :weight bold))))
  `(mode-line-mousable ((t (:background "#000" :foreground "#444"))))
  `(mode-line-mousable-minor-mode ((t (:background "#FFFFFF" :foreground "#888"))))
  `(paren-face ((t (:foreground "#444444"))))
  `(region ((t (:background "#222222"))))
  `(scroll-bar ((t (:background "#000000" :foreground "#444444"))))
  `(show-paren-match-face ((t (:background "#004400" :foreground "#00FF00"))))
  `(show-paren-mismatch-face ((t (:background "Red" :foreground "White"))))
  `(tool-bar ((t (:background "#111" :foreground "#777"))))
  `(tooltip ((t (:background "#333" :foreground "#777"))))
  `(variable-pitch ((t (nil))))
  `(widget-button-face ((t (:bold t :foreground "#888"))))
  `(widget-field-face ((t (:bold t :foreground "#999"))))))

(provide-theme 'dark-ruthless)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; dark-ruthless.el ends here
