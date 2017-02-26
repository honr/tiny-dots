(deftheme light-balcony
  "A relatively serious looking color on light brown theme.")

(let ((black "#000000")
      (red "#bc2020")
      (dark-green "#0c4b05")
      (green "#27831d")
      (blue "#097cbf")
      (yellowish-green "#9cc913")
      (subtle-high "#e4cdc0")
      (yellow-high "#f4f0c5")
      (yellow "#fff94b")
      (pink "#ff59b9")
      (cyan "#0bffe5")
      (white "#ffffff")

      (dark-brown "#431a00")
      (brown "#683718") ;; default foreground
      (light-brown "#977661")
      (comment "#b9a99f")
      (lighter-brown "#e1d5cf")
      (light "#eae5e2") ;; default background
      (lighter "#f2efee")

      (u0 "#0888a6")
      (u1 "#13d2ed")
      (u2 "#bda99c")
      (u3 "#c9ba98")
      (u4 "#decec6")
      (u5 "#eee4de"))

  (custom-theme-set-faces
   'light-balcony
   `(default                       ((t (:foreground ,brown :background ,light))))
   `(cursor                        ((t (:background ,yellowish-green))))
   `(font-lock-builtin-face        ((t (:foreground ,blue))))
   `(font-lock-comment-face        ((t (:foreground ,comment :italic t))))
   `(font-lock-constant-face       ((t (:foreground ,blue))))
   `(font-lock-doc-face            ((t (:foreground ,blue :background ,lighter))))
   `(font-lock-function-name-face  ((t (:foreground ,green :bold t))))
   `(font-lock-keyword-face        ((t (:foreground ,dark-green))))
   `(font-lock-preprocessor-face   ((t (:foreground ,dark-green))))
   `(font-lock-string-face         ((t (:foreground ,dark-brown :background ,lighter-brown))))
   `(font-lock-type-face           ((t (:foreground ,blue))))
   `(font-lock-variable-name-face  ((t (:foreground ,dark-brown :bold t))))
   `(font-lock-warning-face        ((t (:foreground ,red :bold t))))

   `(dired-directory               ((t (:foreground ,blue))))
   `(dired-symlink                 ((t (:foreground ,light-brown))))
   `(dired-ignored                 ((t (:foreground ,comment))))
   `(dired-marked                  ((t (:foreground ,green :bold t))))
   `(dired-flagged                 ((t (:foreground ,light :background ,red :bold t))))
   `(dired-perm-write              ((t (:foreground ,blue :bold t))))
   `(scroll-bar                    ((t (:background ,light-brown))))
   `(fringe                        ((t (:foreground ,comment :background ,light))))
   `(header-line                   ((t (:foreground ,dark-brown :background ,lighter-brown))))
   `(highlight                     ((t (:foreground "light blue" :background "#dddddd"))))
   ;; (highline-face               ((t (:background "SeaGreen")))) ;;
   `(holiday-face                  ((t (:foreground "#777" :background "#000"))))
   `(isearch                       ((t (:foreground ,white :background "#FFBB00"))))
   `(isearch-lazy-highlight-face   ((t (:foreground ,white :background "#ff99bb"))))
   ;; (isearch-secondary           ((t (:foreground "green"))))
   `(menu                          ((t (:foreground ,white :background "#BBBBBB"))))
   `(minibuffer-prompt             ((t (:foreground ,light-brown))))
   `(mode-line                     ((t (:foreground ,white :background ,comment :box (:line-width 1 :color ,light-brown)))))
   `(mode-line-inactive            ((t (:foreground ,light-brown :background ,light :box (:line-width 1 :color ,comment)))))
   `(mode-line-buffer-id           ((t (:foreground ,dark-brown                     :weight bold))))
   `(mode-line-mousable            ((t (:foreground ,brown :background ,black))))
   `(mode-line-mousable-minor-mode ((t (:foreground ,brown :background ,white))))
   `(region                        ((t (:background ,yellow-high))))
   `(secondary-selection           ((t (:foreground "#0088FF" :background "#AACCFF"))))
   `(show-paren-match-face         ((t (:foreground ,green :background "#BBFFBB"))))
   `(show-paren-mismatch-face      ((t (:foreground ,light :background ,red))))
   `(paren-face                    ((t (:foreground ,light-brown))))
   `(tool-bar                      ((t (:foreground "#777" :background "#111"))))
   `(tooltip                       ((t (:foreground "#777" :background "#333"))))
   `(widget-button-face            ((t (:foreground "#888" :bold t))))
   `(widget-field-face             ((t (:foreground "#999" :bold t))))))

(provide-theme 'light-balcony)

;; Local Variables:
;; no-byte-compile: t
;; End:
