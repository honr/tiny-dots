;;; whitestone-serious-theme.el --- custom theme for faces

;; Copyright (C) 2010-2012 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(deftheme whitestone-serious
  "A serious looking black on white theme.")

(custom-theme-set-faces
 'whitestone-serious

 '(default                       ((t (:foreground "#222222" :background "#ffffff"))))
 '(cursor                        ((t (:background "#66cc33"))))

 '(font-lock-builtin-face        ((t (:foreground "#A20"))))
 '(font-lock-comment-face        ((t (:foreground "#999999" :italic t))))
 '(font-lock-constant-face       ((t (:foreground "#A20"))))
 '(font-lock-doc-face            ((t (:foreground "#A20" :background "#EEEEEE"))))
 '(font-lock-function-name-face  ((t (:foreground "#000" :bold t))))
 '(font-lock-keyword-face        ((t (:foreground "#610"))))
 '(font-lock-preprocessor-face   ((t (:foreground "#610"))))
 '(font-lock-string-face         ((t (:foreground "#666666" :background "#EEEEEE"))))
 '(font-lock-type-face           ((t (:foreground "#A20"))))
 '(font-lock-variable-name-face  ((t (:foreground "#222222" :bold t))))
 '(font-lock-warning-face        ((t (:foreground "#D03" :bold t))))

 ;; dired
 '(dired-directory               ((t (:foreground "#06F"))))
 '(dired-symlink                 ((t (:foreground "#808"))))
 '(dired-ignored                 ((t (:foreground "#888"))))
 '(dired-marked                  ((t (:foreground "#080" :bold t))))
 '(dired-flagged                 ((t (:foreground "#FFFFFF" :background "#990000":bold t))))
 '(dired-perm-write              ((t (:foreground "#A20" :bold t))))

 '(scroll-bar                    ((t (:background "#DDDDDD"))))
 '(fringe                        ((t (:foreground "#BBBBBB" :background "#FFFFFF"))))
 ;; (fringe                      ((t (:inherit default))))
 '(header-line                   ((t (:foreground "#888" :background "#dddddd"))))
 '(highlight                     ((t (:foreground "light blue" :background "#dddddd"))))
 ;; (highline-face               ((t (:background "SeaGreen")))) ;;
 '(holiday-face                  ((t (:foreground "#777" :background "#000"))))
 '(isearch                       ((t (:foreground "#ffffff" :background "#FFBB00"))))
 '(isearch-lazy-highlight-face   ((t (:foreground "#ffffff" :background "#ff99bb"))))
 ;; (isearch-secondary           ((t (:foreground "green"))))
 '(menu                          ((t (:foreground "#FFFFFF" :background "#BBBBBB"))))
 '(minibuffer-prompt             ((t (:foreground "#555"))))

 '(mode-line                     ((t (:foreground "#666" :background "#DDDDDD" :box (:line-width 1 :color "#BBBBBB")))))
 '(mode-line-inactive            ((t (:foreground "#999" :background "#EEEEEE" :inherit modeline))))
 '(mode-line-buffer-id           ((t (:foreground "#440" :inherit modeline :weight bold))))
 '(mode-line-mousable            ((t (:foreground "#555" :background "#000"))))
 '(mode-line-mousable-minor-mode ((t (:foreground "#888" :background "#ffffff"))))
 ;; For older versions:
 '(modeline                      ((t (:foreground "#666" :background "#DDDDDD" :box (:line-width 1 :color "#BBBBBB")))))
 '(modeline-inactive             ((t (:foreground "#999" :background "#EEEEEE" :inherit modeline))))
 '(modeline-buffer-id            ((t (:foreground "#440" :inherit modeline :weight bold))))
 '(modeline-mousable             ((t (:foreground "#555" :background "#000"))))
 '(modeline-mousable-minor-mode  ((t (:foreground "#888" :background "#ffffff"))))

 '(region                        ((t (:background "#FFDD66"))))
 '(secondary-selection           ((t (:foreground "#0088FF" :background "#AACCFF"))))
 '(show-paren-match-face         ((t (:foreground "#00BB00" :background "#BBFFBB"))))
 '(show-paren-mismatch-face      ((t (:foreground "White" :background "Red"))))
 '(paren-face                    ((t (:foreground "#AAAAAA"))))
 '(tool-bar                      ((t (:foreground "#777" :background "#111"))))
 '(tooltip                       ((t (:foreground "#777" :background "#333"))))
 ;; (underline                   ((t (:bold t))))
 '(variable-pitch                ((t (nil))))
 '(widget-button-face            ((t (:foreground "#888" :bold t))))
 '(widget-field-face             ((t (:foreground "#999" :bold t)))))

(provide-theme 'whitestone-serious)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; whitestone-serious-theme.el ends here
