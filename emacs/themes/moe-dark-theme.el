;;; moe-dark-theme.el --- An eye-candy theme for Emacser

;; From github.com/kuanyui/moe-theme.el (GPL3) with minor local changes and some
;; omissions.
;; Author: kuanyui<azazabc123@gmail.com>
;; Based on "tango-dark-theme"

;;; Code:

(deftheme moe-dark
  "Face colors for 256 colors terminal (dark background).
Moe, moe, kyun!")

(let ((class '((class color) (min-colors 89)))
      ;; Palette colors.
      (yellow00 "#ffffaf")
      (yellow0 "#ffff87")
      (yellow1 "#fce94f")
      (yellow2 "#ffd700")
      (yellow3 "#c4a000")
      (yellow3-5 "#aaaa11")
      (yellow4 "#875f00")
      (orange1 "#ffaf5f")
      (orange2 "#ff8700")
      (orange3 "#ff5d17")
      (orange4 "#d75f00")
      (orange5 "#af5f00")
      (magenta1 "#ff7bbb")
      (magenta2 "#ff4ea3")
      (magenta3 "#ff1f8b")
      (green1 "#afff00")
      (green2 "#a1db00")
      (green3 "#5faf00")
      (green4 "#008700")
      (green5 "#005f00")
      (cyan1 "#87ffff")
      (cyan2 "#87d7af")
      (cyan3 "#00d7af")
      (cyan4 "#00ac8a")
      (cyan5 "#5faf87")
      (cyan6 "#005f5f")
      (cyan7 "#236f73")
      (blue1 "#5fafd7")
      (blue2 "#1f5bff")
      (blue3 "#005f87")
      (blue4 "#005faf")
      (blue5 "#0000af")
      (blue6 "#00005f")
      (purple1 "#d18aff")
      (purple2 "#af5fff")
      (purple3 "#9a08ff")
      (purple4 "#6c0099")
      (red1 "#ef2929")
      (red2 "#dd0000")
      (red3 "#a40000")
      (red4 "#5f0000")
      (white1 "#c6c6c6")
      (white2 "#c6c6c6")
      (white3 "#b2b2b2")
      (black1 "#a8a8a8")
      (black2 "#8a8a8a")
      (black2-5 "#6c6c6c")
      (black3 "#4e4e4e")
      (black4 "#3a3a3a")
      (black5 "#303030")
      (black6 "#000000")
      (LIGHT_BG "#fdfde7")
      (white0 "#eeeeee")
      (green02 "#5fd700")
      (green01 "#d7ff00")
      (green0 "#d7ff5f")
      (green00 "#d7ff87")
      (cyan0 "#d7ffd7")
      (blue01 "#c3c9f8")
      (blue0 "#afd7ff")
      (blue00 "#d7d7ff")
      (purple0 "#af87ff")
      (purple00 "#e6a8df")
      (red0 "#ff4b4b")
      (red00 "#ffafaf")
      (magenta0 "#ffafd7")
      (magenta00 "#ffd7ff")
      (orange0 "#ffaf87")
      (orange00 "#ffd787")
      (orange000 "#ffd7af")
      (linum-dark "#87875f")
      (linum-light "#d7d7af"))


  (custom-theme-set-faces
   'moe-dark
   ;; Ensure sufficient contrast on low-color terminals.
   `(default ((((class color) (min-colors 4096))
	       (:foreground ,white1 :background ,black5))
	      (((class color) (min-colors 256))
	       (:foreground ,white1 :background ,black5))
	      (,class
	       (:foreground ,white1 :background ,black5))))
   `(cursor ((,class (:background ,white0))))

   ;; Highlighting faces
   `(fringe ((,class (:foreground ,black1 :background ,black5))))
   `(linum ((,class (:foreground ,white3 :background ,black3))))
   `(linum-highlight-face ((,class (:background ,green0 :foreground ,black4))))
   `(highlight ((,class (:background ,black3))))
   `(hl-line ((,class (:background ,black4))))
   `(highlight-symbol-face ((,class (:background ,green5))))
   `(region ((,class (:foreground ,black3 :background ,green0))))
   `(secondary-selection ((,class (:background ,blue3 :foreground ,white0))))
   `(isearch ((,class (:foreground ,white1 :background ,orange3))))
   `(lazy-highlight ((,class (:background ,magenta3 :foreground ,white1))))
   `(trailing-whitespace ((,class (:background ,red3))))
   `(show-paren-match ((,class (:background ,blue3 :foreground nil))))
   `(header-line ((,class (:background ,blue3 :foreground ,white0))))
   `(help-argument-name ((,class (:foreground ,magenta1 :italic t))))
   `(eldoc-highlight-function-argument
     ((,class (:foreground ,green01 :bold t :underline t :background ,green5))))
   ;; Mode line & frames' faces
   `(mode-line ((,class (:box nil :background ,blue0 :foreground ,blue3))))
   `(mode-line-inactive
     ((,class (:box nil :background ,black2-5 :foreground ,white1))))
   `(mode-line-buffer-id
     ((,class (:box nil :foreground ,black5 :background nil :bold t))))
   `(vertical-border ((,class (:foreground ,black3 :background ,black3))))

   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground ,black3 :background ,green2))))
   `(escape-glyph ((,class (:foreground ,yellow3))))
   `(error ((,class (:foreground ,red0))))
   `(warning ((,class (:foreground ,orange1))))
   `(success ((,class (:foreground ,green1))))

   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,purple1))))
   `(font-lock-comment-delimiter-face
     ((,class (:foreground ,black2-5 :slant italic))))
   `(font-lock-comment-face ((,class (:foreground ,black2-5 :slant italic))))
   `(font-lock-constant-face ((,class (:foreground ,blue1))))
   `(font-lock-doc-face ((,class (:foreground ,red0))))
   `(font-lock-doc-string-face ((,class (:foreground ,yellow3))))
   `(font-lock-function-name-face ((,class (:foreground ,yellow2))))
   `(font-lock-keyword-face ((,class (:foreground ,green2))))
   `(font-lock-negation-char-face ((,class (:foreground ,red0))))
   `(font-lock-preprocessor-face ((,class (:foreground ,purple1))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,yellow1))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,purple1))))
   `(font-lock-string-face ((,class (:foreground ,magenta2))))
   `(font-lock-type-face ((,class (:foreground ,cyan3))))
   `(font-lock-variable-name-face ((,class (:foreground ,orange2))))
   `(font-lock-warning-face ((,class (:weight bold :foreground ,red2))))

   ;; Completions
   `(completions-annotations ((,class (:foreground ,green2))))
   `(completions-common-part ((,class (:foreground ,black2-5))))
   `(completions-first-difference
     ((,class (:weight bold :foreground ,orange2))))

   ;; org-mode
   `(org-document-title
     ((,class (:foreground ,blue0 :background ,black5 :weight bold))))
   `(org-document-info
     ((,class (:foreground ,blue1 :background ,black5 :weight bold))))
   `(org-document-info-keyword
     ((,class (:foreground ,orange1 :background ,black2-5))))
   `(org-archived ((,class (:slant italic))))
   `(org-checkbox ((,class (:background ,white2 :foreground ,black3
					:box (:line-width 1 :style released-button)))))
   `(org-date ((,class (:foreground ,blue0 :underline t))))
   `(org-done
     ((,class (:bold t :weight bold :foreground ,green4 :background ,green0
                     :box (:line-width 1 :style none)))))
   `(org-todo
     ((,class (:bold t :weight bold :foreground ,red3 :background ,orange0
                     :box (:line-width 1 :style none)))))
   `(org-level-1 ((,class (:foreground ,blue1))))
   `(org-level-2 ((,class (:foreground ,green2))))
   `(org-level-3 ((,class (:foreground ,orange2))))
   `(org-level-4 ((,class (:foreground ,cyan3))))
   `(org-level-5 ((,class (:foreground ,red1))))
   `(org-level-6 ((,class (:foreground ,purple2))))
   `(org-level-7 ((,class (:foreground ,magenta2))))
   `(org-level-8 ((,class (:foreground ,yellow2))))
   `(org-tag
     ((,class (:background ,black3 :foreground ,white1 :bold t :weight bold))))

   `(org-column ((,class (:background ,black4 :foreground ,black3))))
   `(org-column-title ((,class (:background ,blue0 :foreground ,black5
                                            :underline t :weight bold))))
   `(org-agenda-structure
     ((,class (:foreground ,cyan4 :background nil :bold t))))
   `(org-deadline-announce ((,class (:foreground ,red3))))
   `(org-scheduled ((,class (:foreground ,white3))))
   `(org-scheduled-previously ((,class (:foreground ,red1))))
   `(org-scheduled-today ((,class (:foreground ,blue1))))
   `(org-special-keyword ((,class (:background ,black3 :foreground ,white1))))
   `(org-table ((,class (:background ,black3 :foreground ,white1))))
   `(org-time-grid ((,class (:foreground ,black2))))
   `(org-upcoming-deadline ((,class (:foreground ,red1))))
   `(org-warning ((,class (:bold t :foreground ,white0 :background ,red3))))
   `(org-formula ((,class (:foreground ,purple2))))
   `(org-headline-done ((,class (:foreground ,green2))))
   `(org-hide ((,class (:foreground ,black5))))
   `(org-code ((,class (:foreground ,blue1 :background ,black4))))
   `(org-link ((,class (:foreground ,blue1 :underline t))))
   `(org-footnote ((,class (:foreground ,magenta3))))
   `(org-ellipsis ((,class (:foreground ,red2))))
   `(org-agenda-clocking
     ((,class (:foreground ,blue3 :background ,blue0 :bold t))))
   `(org-agenda-date
     ((,class (:foreground ,blue1 :background ,black5 :underline nil))))
   `(org-agenda-date-weekend
     ((,class (:foreground ,purple1 :underline nil :bold nil))))
   `(org-agenda-date-today ((,class (:foreground ,blue0 :background ,black2-5
                                                 :slant italic :weight bold))))
   `(org-agenda-column-dateline
     ((,class (:foreground ,white0 :background ,black3 :underline t))))
   `(org-agenda-todo ((,class (:foreground ,white0 :background ,red2))))
   `(org-agenda-done ((,class (:foreground ,green2 :background nil))))
   `(org-agenda-dimmed-todo-face
     ((,class (:foreground ,white0 :background ,red2))))
   `(org-priority ((,class (:foreground ,red1 :background ,nil))))
   `(org-block ((,class (:foreground ,blue1 :background ,black4))))
   `(org-block-background ((,class (:foreground nil :background ,black4))))
   `(org-block-begin-line ((,class (:foreground ,white0 :background ,blue3))))
   `(org-block-end-line ((,class (:foreground ,black3 :background ,black4))))
   `(org-quote ((,class (:foreground ,blue1 :background ,black3))))
   `(org-mode-line-clock
     ((,class (:foreground ,blue3 :background ,blue0 :bold t))))
   `(org-mode-line-clock-overrun
     ((,class (:foreground ,white0 :background ,red1 :bold t))))
   `(org-verbatim ((,class (:foreground ,blue1 :background ,black3 :bold nil))))

   ;; outline
   `(outline-1 ((,class (:foreground ,blue1))))
   `(outline-2 ((,class (:foreground ,green2))))
   `(outline-3 ((,class (:foreground ,orange2))))
   `(outline-4 ((,class (:foreground ,cyan3))))
   `(outline-5 ((,class (:foreground ,red1))))
   `(outline-6 ((,class (:foreground ,purple2))))
   `(outline-7 ((,class (:foreground ,magenta2))))
   `(outline-8 ((,class (:foreground ,yellow2))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face ((,class (:foreground ,white1))))
   `(undo-tree-visualizer-current-face
     ((,class (:foreground ,green2 :weight bold))))
   `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,red2))))
   `(undo-tree-visualizer-register-face ((,class (:foreground ,yellow2))))

   ;; Markdown-mode
   `(markdown-blockquote-face
     ((,class (:foreground ,orange2 :background ,black3 :italic t))))
   `(markdown-bold-face ((,class (:foreground ,white1 :bold t))))
   `(markdown-comment-face ((,class (:foreground ,black2 :italic t))))
   `(markdown-header-delimiter-face ((,class (:foreground ,green3 :bold t))))
   `(markdown-header-face ((,class (:foreground ,green2 :bold t))))
   `(markdown-header-rule-face ((,class (:foreground ,green2 :bold t))))
   `(markdown-header-face-1 ((,class (:foreground ,green2 :bold t))))
   `(markdown-header-face-2 ((,class (:foreground ,green2 :bold t))))
   `(markdown-header-face-3 ((,class (:foreground ,green2 :bold t))))
   `(markdown-header-face-4 ((,class (:foreground ,green2 :bold t))))
   `(markdown-header-face-5 ((,class (:foreground ,green2 :bold t))))
   `(markdown-header-face-6 ((,class (:foreground ,green2 :bold t))))
   `(markdown-link-face ((,class (:foreground ,magenta1 :underline t))))
   `(markdown-inline-code-face
     ((,class (:foreground ,blue1 :background ,black3))))
   `(markdown-italic-face
     ((,class (:foreground ,white1 :italic t :underline ,white1))))
   `(markdown-list-face
     ((,class (:foreground ,green2 :background ,black5 :bold t))))
   `(markdown-math-face ((,class (:foreground ,magenta1))))
   `(markdown-missing-link-face ((,class (:foreground ,red1 :bold t))))
   `(markdown-pre-face ((,class (:foreground ,blue1))))
   `(markdown-reference-face ((,class (:foreground ,orange2 :italic t))))
   `(markdown-url-face ((,class (:foreground ,magenta3 :underline ,magenta3))))

   ;; Jabber
   `(jabber-activity-face ((,class (:foreground ,magenta2))))
   `(jabber-activity-personal-face ((,class (:foreground ,cyan3))))
   `(jabber-chat-error ((,class (:foreground ,red00 :background ,red2))))
   `(jabber-chat-prompt-foreign ((,class (:foreground ,orange2))))
   `(jabber-chat-prompt-local ((,class (:foreground ,blue1))))
   `(jabber-chat-prompt-system ((,class (:foreground ,yellow2 :weight bold))))
   `(jabber-chat-text-foreign ((,class (:foreground ,white0))))
   `(jabber-chat-text-local ((,class (:foreground ,white3))))
   `(jabber-rare-time-face ((,class (:foreground ,black1))))
   `(jabber-roster-user-away ((,class (:foreground ,orange2))))
   `(jabber-roster-user-chatty ((,class (:foreground ,purple1))))
   `(jabber-roster-user-dnd ((,class (:foreground ,red1))))
   `(jabber-roster-user-error ((,class (:foreground ,red00 :background ,red3))))
   `(jabber-roster-user-offline ((,class (:foreground ,black1))))
   `(jabber-roster-user-online ((,class (:foreground ,green2))))
   `(jabber-roster-user-xa ((,class (:foreground ,black1))))

   ;; Button and link faces
   `(link ((,class (:underline t :foreground ,blue1))))
   `(link-visited ((,class (:underline t :foreground ,blue2))))

   ;; Dired/Dired+
   `(direddirectory ((,class (:foreground ,blue1 :bold t))))
   `(diredflagged ((,class (:foreground ,red1))))
   `(diredheader ((,class (:foreground ,black5 :background ,green2 :bold t))))
   `(diredignored ((,class (:foreground ,black1))))
   `(diredmark ((,class (:foreground ,green1))))
   `(diredmarked ((,class (:foreground ,green2))))
   `(diredperm-write ((,class (:foreground ,red2 :bold t))))
   `(diredsymlink ((,class (:foreground ,magenta2))))
   `(diredwarning ((,class (:foreground ,white1 :background ,red3 :bold t))))
   `(diredp-compressed-file-suffix ((,class (:foreground ,purple2))))
   `(diredp-date-time ((,class (:foreground ,blue1 :background ,black4))))
   `(diredp-deletion ((,class (:foreground ,white1, :background ,red3))))
   `(diredp-deletion-file-name ((,class (:foreground ,red2))))
   `(diredp-dir-heading ((,class (:foreground ,black5 :background ,green2))))
   `(diredp-dir-priv
     ((,class (:foreground ,blue1 :background ,black5 :bold t))))
   `(diredp-display-msg ((,class (:foreground ,orange2))))
   `(diredp-executable-tag ((,class (:foreground ,green2))))
   `(diredp-file-name ((,class (:foreground ,white1))))
   `(diredp-file-suffix ((,class (:foreground ,orange2))))
   `(diredp-flag-mark
     ((,class (:foreground ,white1 :background ,magenta3 :bold t))))
   `(diredp-flag-mark-line
     ((,class (:foreground ,black5 :background ,magenta1))))
   `(diredp-ignoredfile-name ((,class (:foreground ,black2))))
   `(diredp-link-priv ((,class (:foreground ,magenta3))))
   `(diredp-mode-line-flagged
     ((,class (:foreground ,black5 :background ,green2))))
   `(diredp-mode-line-marked
     ((,class (:foreground ,white1 :background ,magenta3 bold t))))
   `(diredp-no-priv ((,class (:foreground ,white1 :background ,black4))))
   `(diredp-number ((,class (:foreground ,yellow1))))
   `(diredp-other-priv ((,class (:foreground ,white1 :background ,blue3))))
   `(diredp-rare-priv ((,class (:foreground ,white1 :background ,purple2))))
   `(diredp-symlink ((,class (:foreground ,magenta3))))
   `(diredp-read-priv ((,class (:foreground ,green4 :background ,green0))))
   `(diredp-write-priv ((,class (:foreground ,blue5 :background ,blue0))))
   `(diredp-exec-priv ((,class (:foreground ,red3 :background ,orange0))))

   ;; Magit >= 2.1.0
   `(magit-bisect-bad ((,class (:foreground ,red3 :background ,red00))))
   `(magit-bisect-good ((,class (:foreground ,green4 :background ,green0))))
   `(magit-bisect-skip ((,class (:foreground ,yellow4 :background ,orange00))))
   `(magit-blame-date ((,class (:foreground ,orange2 :background ,black3))))
   `(magit-blame-hash ((,class (:foreground ,orange2 :background ,black3))))
   `(magit-blame-heading ((,class (:foreground ,white1 :background ,black3))))
   `(magit-blame-name ((,class (:foreground ,green1 :background ,black3))))
   `(magit-blame-summary ((,class (:foreground ,white2 :background ,black3))))
   `(magit-branch-current ((,class (:foreground ,white0 :background ,orange2 :bold t :underline nil))))
   `(magit-branch-local ((,class (:foreground ,green4 :background ,green00 :bold t :underline t))))
   `(magit-branch-remote ((,class (:foreground ,blue3 :background ,blue0 :bold t :underline t))))
   `(magit-cherry-equivalent ((,class (:foreground ,purple2))))
   `(magit-cherry-unmatched ((,class (:foreground ,cyan4))))
   `(magit-diff-added ((,class (:foreground ,green0 :background nil :bold t))))
   `(magit-diff-added-highlight ((,class (:foreground ,green0 :bold t :inherit (magit-section-highlight)))))
   `(magit-diff-base ((,class (:foreground ,yellow1 :background nil))))
   `(magit-diff-base-highlight ((,class (:foreground ,yellow1 :bold t :inherit (magit-section-highlight)))))
   `(magit-diff-conflict-heading ((,class (:foreground ,white0 :background ,orange2))))
   `(magit-diff-context ((,class (:foreground ,white1 :background nil))))
   `(magit-diff-context-highlight ((,class (:foreground ,white1 :inherit (magit-section-highlight)))))
   `(magit-diff-file-heading ((,class (:foreground ,white0 :bold t))))
   `(magit-diff-file-heading-highlight ((,class (:background ,blue3 :bold t))))
   `(magit-diff-file-heading-selection ((,class (:foreground ,white0 :background ,black2 :bold t))))
   `(magit-diff-hunk-heading ((,class (:foreground ,black2 :background ,black3 :bold t))))
   `(magit-diff-hunk-heading-highlight ((,class (:foreground ,black4 :background ,blue01))))
   `(magit-diff-hunk-heading-selection ((,class (:foreground ,white1 :background ,white1))))
   `(magit-diff-lines-boundary ((,class (:foreground ,white1 :background ,red0))))
   `(magit-diff-lines-heading ((,class (:foreground ,white1 :background ,red0))))
   `(magit-diff-our ((,class (:foreground ,magenta3))))
   `(magit-diff-our-highlight ((,class (:foreground ,magenta3 :background ,magenta00))))
   `(magit-diff-removed ((,class (:foreground ,red0 :background nil :bold t))))
   `(magit-diff-removed-highlight ((,class (:foreground ,red0 :bold t :inherit (magit-section-highlight)))))
   `(magit-diff-their ((,class (:foreground ,cyan4)))) ;
   `(magit-diff-their-highlight ((,class (:foreground ,cyan6 :background ,cyan1))))
   `(magit-diff-whitespace-warning ((,class (:foreground ,red3 :background ,red3))))
   `(magit-diffstat-added ((,class (:foreground ,green3 :background ,green0 :bold t))))
   `(magit-diffstat-removed ((,class (:foreground ,red3 :background ,red0 :bold t))))
   `(magit-dimmed ((,class (:foreground ,black2))))
   `(magit-filename ((,class (:foreground ,blue1))))
   `(magit-hash ((,class (:foreground ,orange2 :background nil))))
   `(magit-head ((,class (:foreground ,orange4 :background ,orange000))))
   `(magit-header-line ((,class (:foreground ,orange2))))
   `(magit-log-author ((,class (:foreground ,green2 :background nil))))
   `(magit-log-date ((,class (:foreground ,black2 :background nil))))
   `(magit-log-graph ((,class (:foreground ,black2 :background nil))))
   `(magit-process-ng ((,class (:foreground ,red3 :background ,red00 :underline t))))
   `(magit-process-ok ((,class (:foreground ,green3 :background ,green00 :underline t))))
   `(magit-reflog-amend ((,class (:foreground ,magenta3 :background ,magenta00))))
   `(magit-reflog-checkout ((,class (:foreground ,green3 :background ,green00))))
   `(magit-reflog-cherry-pick ((,class (:foreground ,orange4 :background ,orange00))))
   `(magit-reflog-commit ((,class (:foreground ,yellow4 :background ,yellow0))))
   `(magit-reflog-merge ((,class (:foreground ,purple4 :background ,purple00))))
   `(magit-reflog-other ((,class (:foreground ,white0 :background ,black3))))
   `(magit-reflog-rebase ((,class (:foreground ,cyan6 :background ,cyan2))))
   `(magit-reflog-remote ((,class (:foreground ,blue4 :background ,blue0))))
   `(magit-reflog-reset ((,class (:foreground ,red3 :background ,red00))))
   `(magit-section-heading ((,class (:foreground ,orange3 :background nil :bold t :underline t))))
   `(magit-section-heading-selection ((,class (:background ,blue0))))
   `(magit-section-highlight ((,class (:background ,black3))))
   `(magit-sequence-done ((,class (:foreground ,green3))))
   `(magit-sequence-drop ((,class (:foreground ,orange2))))
   `(magit-sequence-head ((,class (:foreground ,blue2))))
   `(magit-sequence-onto ((,class (:foreground ,purple2))))
   `(magit-sequence-part ((,class (:foreground ,cyan4))))
   `(magit-sequence-pick ((,class (:foreground ,magenta2))))
   `(magit-sequence-stop ((,class (:foreground ,red3))))
   `(magit-signature-bad ((,class (:foreground ,red2))))
   `(magit-signature-good ((,class (:foreground ,green3))))
   `(magit-signature-untrusted ((,class (:foreground ,yellow3))))
   `(magit-tag ((,class (:foreground ,blue3 :background ,blue0))))
   `(magit-valid-signature ((,class (:foreground ,cyan4 :background ,LIGHT_BG :bold t))))
   `(magit-whitespace-warning-face ((,class (:foreground ,white0 :background ,red2 :bold t))))

   ;; Magit
   `(magit-branch ((,class (:foreground ,green4 :background ,green01 :bold t :underline t))))
   `(magit-diff-add ((,class (:foreground ,green0 :background nil :bold t))))
   `(magit-diff-del ((,class (:foreground ,red0 :background nil :bold t))))
   `(magit-diff-file-header ((,class (:foreground ,white0 :background ,black2 :bold t))))
   `(magit-diff-hunk-header ((,class (:foreground ,white2 :background ,black2-5 :bold t))))
   `(magit-diff-merge-current ((,class (:foreground ,purple1))))
   `(magit-diff-merge-diff3-separator ((,class (:foreground ,purple1))))
   `(magit-diff-merge-proposed ((,class (:foreground ,purple1))))
   `(magit-diff-merge-separator ((,class (:foreground ,purple1))))
   `(magit-diff-none ((,class (:foreground ,black2))))
   `(magit-header ((,class (:foreground ,blue2 :background ,white0 :underline ,blue2))))
   `(magit-item-highlight ((,class (:background "#444444" :foreground ,white0))))
   `(magit-item-mark ((,class (:foreground ,white0 :background ,blue3))))
   `(magit-log-author ((,class (:foreground ,green0 :background ,black4))))
   `(magit-log-author-date-cutoff ((,class (:foreground ,red1 :bold t))))
   `(magit-log-date ((,class (:foreground ,white1 :background ,black4))))
   `(magit-log-graph ((,class (:foreground ,white2 :background ,black4))))
   `(magit-log-head-label-bisect-bad ((,class (:foreground ,red3 :background ,red00))))
   `(magit-log-head-label-bisect-good ((,class (:foreground ,green4 :background ,green0))))
   `(magit-log-head-label-bisect-skip ((,class (:foreground ,yellow3 :background ,orange00))))
   `(magit-log-head-label-default ((,class (:foreground ,green4 :background ,green2 :bold t :underline t))))
   `(magit-log-head-label-head ((,class (:foreground ,green4 :background ,green01 :bold t :underline t))))
   `(magit-log-head-label-local ((,class (:foreground ,green4 :background ,green00 :bold t :underline t))))
   `(magit-log-head-label-patches ((,class (:foreground ,orange4 :background ,orange0 :bold t :underline t))))
   `(magit-log-head-label-remote ((,class (:foreground ,blue4 :background ,blue0 :bold t :underline t))))
   `(magit-log-head-label-tags ((,class (:foreground ,yellow4 :background ,yellow00 :bold t :underline t))))
   `(magit-log-head-label-wip ((,class (:foreground ,white2 :background ,black2))))
   `(magit-log-message ((,class (:foreground ,white1 :background nil))))
   `(magit-log-reflog-label-amend ((,class (:foreground ,magenta3 :background ,magenta0))))
   `(magit-log-reflog-label-checkout ((,class (:foreground ,green4 :background ,green00))))
   `(magit-log-reflog-label-cherry-pick ((,class (:foreground ,orange4 :background ,orange00))))
   `(magit-log-reflog-label-commit ((,class (:foreground ,yellow4 :background ,yellow0))))
   `(magit-log-reflog-label-merge ((,class (:foreground ,purple4 :background ,purple0))))
   `(magit-log-reflog-label-other ((,class (:foreground ,white0 :background ,black3))))
   `(magit-log-reflog-label-rebase ((,class (:foreground ,cyan6 :background ,cyan2))))
   `(magit-log-reflog-label-remote ((,class (:foreground ,blue4 :background ,blue0))))
   `(magit-log-reflog-label-reset ((,class (:foreground ,red3 :background ,red00))))
   `(magit-log-sha1 ((,class (:foreground ,orange2 :background ,black4))))
   `(magit-process-ng ((,class (:foreground ,red3 :background ,red00 :underline t :bold t))))
   `(magit-process-ok ((,class (:foreground ,green4 :background ,green00 :underline t :bold t))))
   `(magit-section-title ((,class (:foreground ,orange2 :background ,black5 :underline t :bold ,t))))
   `(magit-signature-bad ((,class (:foreground ,red1))))
   `(magit-signature-good ((,class (:foreground ,green1))))
   `(magit-signature-none ((,class (:foreground ,white1))))
   `(magit-signature-untrusted ((,class (:foreground ,cyan3))))
   `(magit-tag ((,class (:foreground ,blue3 :background ,blue0))))
   `(magit-valid-signature ((,class (:foreground ,cyan3 :background ,black5 :bold t))))
   `(magit-whitespace-warning-face ((,class (:foreground ,white0 :background ,red3 :bold t))))

   ;; git-commit-mode
   `(git-commit-branch-face ((,class (:foreground ,blue4 :background ,white0 :bold t :underline t))))
   `(git-commit-comment-action-face ((,class (:foreground ,orange2 :background ,black5 :underline t))))
   `(git-commit-comment-file-face ((,class (:foreground ,magenta2))))
   `(git-commit-comment-heading-face ((,class (:foreground ,green01 :background ,black3 :bold t))))
   `(git-commit-known-pseudo-header-face ((,class (:foreground ,green1))))
   `(git-commit-no-branch-face ((,class (:foreground ,orange3))))
   `(git-commit-nonempty-second-line-face ((,class (:foreground ,red2))))
   `(git-commit-note-face ((,class (:foreground ,cyan3))))
   `(git-commit-overlong-summary-face ((,class (:foreground ,red2))))
   `(git-commit-pseudo-header-face ((,class (:foreground ,magenta3))))
   `(git-commit-summary-face ((,class (:foreground ,blue1))))
   `(git-rebase-description-face ((,class (:foreground ,black3))))
   `(git-rebase-killed-action-face ((,class (:foreground ,black3))))

   ;; Message faces
   `(message-cited-text ((,class (:foreground ,green1))))
   `(message-header-cc ((,class (:foreground ,blue0))))
   `(message-header-name ((,class (:foreground ,white3))))
   `(message-header-newsgroups ((,class (:foreground ,blue1 :bold t))))
   `(message-header-other ((,class (:foreground ,magenta1))))
   `(message-header-subject ((,class (:foreground ,white0 :bold t))))
   `(message-header-to ((,class (:foreground ,blue1 :underline t :bold t))))
   `(message-header-xheader ((,class (:foreground ,black1))))
   `(message-mml ((,class (:foreground ,orange2))))
   `(message-separator ((,class (:foreground ,black2-5))))

   ;; Grep
   `(grep-context-face ((,class (:foreground ,red2))))
   `(grep-error-face ((,class (:foreground ,red1 :weight bold :underline t))))
   `(grep-hit-face ((,class (:foreground ,green2))))
   `(grep-match-face ((,class
                       (:foreground nil :background nil :inherit match))))

   ;; Diff
   `(diff-added ((,class (:foreground ,green0 :background ,black4 :bold t))))
   `(diff-changed ((,class (:foreground ,yellow2 :background ,black4 :bold t))))
   `(diff-context ((,class (:foreground ,black2-5))))
   `(diff-file-header ((,class (:foreground ,black5 :background ,blue0 :bold t))))
   `(diff-function ((,class (:foreground ,black4 :background ,white2))))
   `(diff-header ((,class (:foreground ,blue3 :background ,blue0))))
   `(diff-hunk-header ((,class (:foreground ,blue0 :background ,black3 :bold t))))
   `(diff-index ((,class (:foreground ,black4 :background ,white3 :bold t))))
   `(diff-indicator-added ((,class (:foreground ,white0 :background ,green3 :bold t))))
   `(diff-indicator-changed ((,class (:foreground ,white0 :background ,yellow3 :bold t))))
   `(diff-indicator-removed ((,class (:foreground ,white0 :background ,red3 :bold t))))
   `(diff-nonexistent ((,class (:foreground ,white0 :background ,red3 :bold t))))
   `(diff-refine-added ((,class (:foreground ,white0 :background ,green4 :bold t))))
   `(diff-refine-change ((,class (:foreground ,white0 :background ,yellow4 :bold t))))
   `(diff-refine-removed ((,class (:foreground ,white0 :background ,red3 :bold t))))
   `(diff-removed ((,class (:foreground ,red0 :background ,black4 :bold t))))

   ;; Ediff
   `(ediff-current-diff-A ((,class (:background ,yellow4))))
   `(ediff-current-diff-Ancestor ((,class (:background ,blue3))))
   `(ediff-current-diff-B ((,class (:background ,purple4))))
   `(ediff-current-diff-C ((,class (:background ,orange5))))
   `(ediff-even-diff-A ((,class (:background ,black3))))
   `(ediff-even-diff-Ancestor ((,class (:background ,black3))))
   `(ediff-even-diff-B ((,class (:background ,black3))))
   `(ediff-even-diff-C ((,class (:background ,black3))))
   `(ediff-fine-diff-A ((,class (:foreground nil :background ,green5 :bold t))))
   `(ediff-fine-diff-Ancestor ((,class (:foreground nil :background ,green5 :bold t))))
   `(ediff-fine-diff-B ((,class (:foreground nil :background ,green5 :bold t))))
   `(ediff-fine-diff-C ((,class (:foreground nil :background ,green5 :bold t))))
   `(ediff-odd-diff-A ((,class (:background ,red3))))
   `(ediff-odd-diff-Ancestor ((,class (:background ,red3))))
   `(ediff-odd-diff-B ((,class (:background ,red3))))
   `(ediff-odd-diff-C ((,class (:background ,red3))))

   ;; smerge
   `(smerge-refined-change ((,class (:background ,blue3 :foreground ,white0))))

   ;; Flyspell faces
   `(flyspell-duplicate ((,class (:underline t foreground-color,orange1))))
   `(flyspell-incorrect ((,class (:background ,red1 :foreground ,white1 :bold t))))

   ;; EShell
   `(eshell-ls-archive ((,class (:foreground ,purple2))))
   `(eshell-ls-backup ((,class (:foreground ,black3))))
   `(eshell-ls-clutter ((,class (:foreground ,black2))))
   `(eshell-ls-directory ((,class (:foreground ,blue1 :bold t))))
   `(eshell-ls-executable ((,class (:foreground ,green2))))
   `(eshell-ls-missing ((,class (:foreground ,white0 :background ,red3))))
   `(eshell-ls-product ((,class (:foreground ,white0 :background ,green2))))
   `(eshell-ls-readonly ((,class (:foreground ,orange3))))
   `(eshell-ls-special ((,class (:foreground ,yellow1))))
   `(eshell-ls-symlink  ((,class (:foreground ,magenta2))))
   `(eshell-ls-unreadable ((,class (:foreground ,black2))))
   `(eshell-prompt ((,class (:foreground ,white0 :background ,black2-5 :bold t))))

   ;; Comint prompt
   `(comint-highlight-prompt ((,class (:foreground ,white0 :background ,black2-5 :bold t))))

   ;; which-function-mode
   `(which-func ((,class (:foreground ,white0 :background ,orange2))))

   ;; Flymake
   `(flymake-warnline ((,class (:underline ,orange2))))
   `(flymake-errline ((,class (:underline ,red2))))

   ;; Flycheck
   `(flycheck-error ((,class (:background ,red2 :foreground ,white0 :bold t))))
   `(flycheck-warnline ((,class (:background ,orange2 :foreground ,white0 :bold t))))

   ;; MMM-Mode
   `(mmm-cleanup-submode-face ((,class (:background ,orange00))))
   `(mmm-code-submode-face ((,class (:background ,blue00))))
   `(mmm-comment-submode-face ((,class (:background ,blue0))))
   `(mmm-declaration-submode-face ((,class (:background ,cyan1))))
   `(mmm-default-submode-face ((,class (:background nil))))
   `(mmm-delimiter-face ((,class (:background nil :foreground ,white0))))
   `(mmm-init-submode-face ((,class (:background ,magenta0))))
   `(mmm-output-submode-face ((,class (:background ,purple00))))
   `(mmm-special-submode-face ((,class (:background ,green00))))

   ;; Clojure/Cider
   `(clojure-test-failure-face ((,class (:underline ,orange2))))
   `(clojure-test-error-face ((,class (:underline ,red2))))
   `(clojure-test-success-face ((,class (:underline ,green3))))
   `(cider-deprecated-face ((,class (:background ,red4))))

   ;; Javascript
   `(js2-function-param-face ((,class (:foreground ,green3))))
   `(js2-external-variable ((,class (:foreground ,orange2 :underline t))))
   `(js2-error ((,class (:foreground ,red2 :underline t :bold t))))
   `(js2-warning ((,class (:foreground nil :underline t :bold t))))

   ;; ERC
   `(erc-button ((,class (:foreground ,blue1 :underline ,blue1 :bold nil))))
   `(erc-current-nick-face ((,class (:foreground ,green1))))
   `(erc-dangerous-hosts ((,class (:foreground ,red2 :bold t))))
   `(erc-direct-msg-face ((,class (:foreground ,orange2))))
   `(erc-error-face ((,class (:foreground ,red2))))
   `(erc-header-face ((,class (:background ,blue1))))
   `(erc-input-face ((,class (:foreground ,white0))))
   `(erc-keyword-face ((,class (:foreground ,magenta2 :bold t))))
   `(erc-my-nick-face ((,class (:foreground ,green1 :bold t))))
   `(erc-nick-default-face ((,class (:bold t :foreground ,blue1))))
   `(erc-nick-msg-face ((,class (:weight normal :foreground ,orange2))))
   `(erc-notice-face ((,class (:foreground ,black2))))
   `(erc-pal-face ((,class (:foreground ,purple1))))
   `(erc-prompt-face ((,class (:bold t :foreground ,green01 :background ,black2-5))))
   `(erc-timestamp-face ((,class (:foreground ,orange2))))

   ;; ansi-term
   `(term-color-black ((,class (:background ,black5 :foreground ,black5))))
   `(term-color-blue ((,class (:background ,cyan3 :foreground ,cyan3))))
   `(term-color-cyan ((,class (:background ,cyan3 :foreground ,cyan3))))
   `(term-color-green ((,class (:background ,green2 :foreground ,green2))))
   `(term-color-magenta ((,class (:background ,magenta3 :foreground ,magenta3))))
   `(term-color-red ((,class (:background ,red1 :foreground ,red1))))
   `(term-color-white ((,class (:background ,white0 :foreground ,white0))))
   `(term-color-yellow ((,class (:background ,orange2 :foreground ,orange2))))

   ;; ido-mode
   `(ido-first-match ((,class (:foreground ,orange2 :bold t))))
   `(ido-incomplete-regexp ((,class (:foreground ,red0 :bold t))))
   `(ido-indicator ((,class (:foreground ,yellow4 :background ,orange00))))
   `(ido-only-match ((,class (:foreground ,green2 :background nil))))
   `(ido-subdir ((,class (:foreground ,blue1 :bold t))))
   `(ido-virtual ((,class (:foreground ,magenta3))))

   ;; notmuch
   `(notmuch-search-unread-face ((,class (:weight bold))))

   ;; Column marker
   `(column-marker-1 ((,class (:background ,black2-5))))
   `(column-marker-2 ((,class (:background ,yellow4))))
   `(column-marker-3 ((,class (:background ,red3))))

   ;; widget
   `(widget-button ((,class (:foreground ,blue1 :underline t :bold t))))
   `(widget-button-pressed ((,class (:foreground ,blue0))))
   `(widget-documentation ((,class (:foreground ,green1))))
   `(widget-field ((,class (:foreground ,green0 :background ,black3))))
   `(widget-inactive ((,class (:foreground ,black2))))
   `(widget-single-line-field ((,class (:foreground ,green0 :background ,black3))))

   ;; table
   `(table-cell ((,class (:foreground ,white0 :background ,black3))))

   ;; compilation
   `(compilation-column-number ((,class (:foreground ,green2))))
   `(compilation-error ((,class (:foreground ,red3 :background ,red0 :bold t))))
   `(compilation-info ((,class (:foreground ,orange2 :background ,black3))))
   `(compilation-line-number ((,class (:foreground ,blue1))))
   `(compilation-mode-line-exit ((,class (:foreground ,green4 :background ,green0 :bold t))))
   `(compilation-mode-line-fail ((,class (:foreground ,red3 :background ,red00 :bold t))))
   `(compilation-mode-line-run ((,class (:foreground ,orange4 :background ,orange00 :bold t))))
   `(compilation-warning ((,class (:foreground ,orange3))))

   ;; info
   `(info-header-node ((,class (:foreground ,magenta2 :bold t))))
   `(info-header-xref ((,class (:foreground ,blue1 :background nil :bold t))))
   `(info-index-match ((,class (:background ,blue3))))
   `(info-menu-header ((,class (:foreground ,white0 :bold t :underline t))))
   `(info-menu-star ((,class (:foreground ,red1))))
   `(info-node ((,class (:foreground ,red3))))
   `(info-title-1 ((,class (:foreground ,blue1 :bold t))))
   `(info-title-2 ((,class (:foreground ,green2 :bold t))))
   `(info-title-3 ((,class (:foreground ,orange2 :bold t))))
   `(info-title-4 ((,class (:foreground ,magenta2 :bold t))))
   `(info-xref ((,class (:foreground ,blue1 :underline t))))
   `(info-xref-visited ((,class (:foreground ,purple1 :underline t))))

   ;; Haskell
   `(haskell-interactive-face-compile-error ((,class (:foreground ,red0 :background nil :bold t))))
   `(haskell-interactive-face-compile-warning ((,class (:foreground ,orange2 :background nil :bold t))))
   `(haskell-interactive-face-garbage ((,class (:foreground ,black2))))
   `(haskell-interactive-face-prompt ((,class (:foreground ,green01 :background ,black2-5 :bold t))))
   `(haskell-interactive-face-result ((,class (:foreground ,blue1))))

   ;; EMMS
   `(emms-state-current-playing-time ((,class (:foreground ,blue3 :bold t))))
   `(emms-state-total-playing-time ((,class (:foreground ,blue3))))
   `(emms-playlist-selected-face ((,class (:foreground ,green2 :bold t))))
   `(emms-playlist-track-face ((,class (:foreground ,cyan5))))

   ;; tty-menu
   `(menu ((,class (:foreground ,black1 :background ,black3))))
   `(tty-menu-disabled-face ((,class (:foreground ,black2-5 :background ,black3))))
   `(tty-menu-enabled-face ((,class (:foreground ,white0 :background ,black3 :bold t))))
   `(tty-menu-selected-face ((,class (:background ,blue3))))

   ;; web-mode
   `(web-mode-comment-face ((,class (:foreground ,black2-5))))
   `(web-mode-current-element-highlight-face ((,class (:background ,black3))))
   `(web-mode-current-column-highlight-face ((,class (:background ,black3))))
   `(web-mode-symbol-face ((,class (:foreground ,yellow2))))
   `(web-mode-type-face ((,class (:foreground ,cyan3))))

   ;; Custom
   `(custom-button ((,class (:background ,blue0 :foreground ,blue3
					 :box (:line-width 1 :style released-button)))))
   `(custom-button-mouse ((,class (:background ,blue00 :foreground ,black3
					       :box (:line-width 1 :style released-button)))))
   `(custom-button-pressed ((,class (:foreground ,black3 :background ,white1
						 :box (:line-width 1 :style pressed-button)))))

   ;; Hydra
   `(hydra-face-red ((,class (:foreground ,red0))))
   `(hydra-face-blue ((,class (:foreground ,blue1))))
   `(hydra-face-amaranth ((,class (:foreground ,magenta2))))

  (custom-theme-set-variables
   'moe-dark
   `(ansi-color-names-vector [,black5 ,red0 ,green0 ,yellow1
                                      ,blue1 ,purple1 ,blue0 ,white1])))


(setq moe-theme-which-enabled 'dark)

(provide-theme 'moe-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; moe-dark-theme.el ends here
