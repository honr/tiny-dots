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
 
 '(default ((t (:background "#ffffff" :foreground "#222222"))))
 '(cursor ((t (:background "#66cc33"))))

 '(font-lock-builtin-face ((t (:foreground "#A20"))))
 '(font-lock-comment-face ((t (:italic t :foreground "#999999"))))
 '(font-lock-constant-face ((t (:foreground "#A20"))))
 '(font-lock-doc-face ((t (:foreground "#A20" :background "#EEEEEE"))))
 '(font-lock-function-name-face ((t (:bold t :foreground "#000"))))
 '(font-lock-keyword-face ((t (:foreground "#610"))))
 '(font-lock-preprocessor-face ((t (:foreground "#610"))))
 '(font-lock-string-face ((t (:foreground "#666666" :background "#EEEEEE"))))
 '(font-lock-type-face ((t (:foreground "#A20"))))
 '(font-lock-variable-name-face ((t (:bold t))))
 '(font-lock-warning-face ((t (:bold t :foreground "#D03"))))
 '
 ;; dired
 '(dired-directory ((t (:foreground "#06F"))))
 '(dired-symlink ((t (:foreground "#808"))))
 '(dired-ignored ((t (:foreground "#888"))))
 '(dired-marked ((t (:bold t :foreground "#080"))))
 '(dired-flagged ((t (:bold t :foreground "#FFFFFF" :background "#990000"))))
 '(dired-perm-write ((t (:bold t :foreground "#A20"))))
 
 '(scroll-bar ((t (:background "#DDDDDD"))))
 '(fringe ((t (:background "#FFFFFF" :foreground "#BBBBBB"))))
 ;; (fringe ((t (:inherit default))))
 '(header-line ((t (:background "#dddddd" :foreground "#888"))))
 '(highlight ((t (:background "#dddddd" :foreground "light blue"))))
 ;; (highline-face ((t (:background "SeaGreen")))) ;;
 '(holiday-face ((t (:background "#000" :foreground "#777"))))
 '(isearch ((t (:background "#FFBB00" :foreground "#ffffff"))))
 '(isearch-lazy-highlight-face ((t (:background "#ff99bb" :foreground "#ffffff"))))
 ;; (isearch-secondary ((t (:foreground "green"))))
 '(menu ((t (:background "#BBBBBB" :foreground "#FFFFFF"))))
 '(minibuffer-prompt ((t (:foreground "#555"))))
 '(modeline ((t (:background "#DDDDDD" :foreground "#666" :box (:line-width 1 :color "#BBBBBB")))))
 '(mode-line-inactive ((t (:inherit modeline :background "#EEEEEE" :foreground "#999"))))
 '(modeline-buffer-id ((t (:inherit modeline :foreground "#440" :weight bold))))
 '(modeline-mousable ((t (:background "#000" :foreground "#555"))))
 '(modeline-mousable-minor-mode ((t (:background "#ffffff" :foreground "#888"))))
 '(region ((t (:background "#FFDD66"))))
 '(secondary-selection ((t (:background "#AACCFF" :foreground "#0088FF"))))
 '(show-paren-match-face ((t (:background "#BBFFBB" :foreground "#00BB00"))))
 '(show-paren-mismatch-face ((t (:background "Red" :foreground "White"))))
 '(paren-face ((t (:foreground "#AAAAAA"))))
 '(tool-bar ((t (:background "#111" :foreground "#777"))))
 '(tooltip ((t (:background "#333" :foreground "#777"))))
 ;; (underline ((t (:bold t))))
 '(variable-pitch ((t (nil))))
 '(widget-button-face ((t (:bold t :foreground "#888"))))
 '(widget-field-face ((t (:bold t :foreground "#999")))))

(provide-theme 'whitestone-serious)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; whitestone-serious-theme.el ends here
