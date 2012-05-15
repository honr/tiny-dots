;;; fruitsalad-dark-theme.el --- custom theme for faces

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

(deftheme fruitsalad-dark
  "A color on black theme, resembling fruitsalad in a dark bowl.")

(custom-theme-set-faces
 'fruitsalad-dark
 '(default ((t (:background "#222222" :foreground "#EEEEEE"))))
 '(cursor ((t (:background "#88FF44"))))

 '(font-lock-builtin-face ((t (:foreground "#F8C"))))
 '(font-lock-comment-face ((t (:italic t :foreground "#777"))))
 '(font-lock-constant-face ((t (:foreground "#0CC"))))
 '(font-lock-doc-face ((t (:foreground "#7AA" :background "#333"))))
 '(font-lock-function-name-face ((t (:foreground "#FFFFFF"))))
 '(font-lock-keyword-face ((t (:foreground "#8CF"))))
 '(font-lock-preprocessor-face ((t (:foreground "#8CF"))))
 '(font-lock-string-face ((t (:foreground "#0AA" :background "#183333"))))
 '(font-lock-type-face ((t (:foreground "#9D4"))))
 '(font-lock-variable-name-face ((t (:foreground "#FFCC88"))))
 '(font-lock-warning-face ((t (:bold t :foreground "Pink"))))
       
 '(dired-directory ((t (:foreground "#4AF"))))
 '(dired-symlink ((t (:foreground "#F8F"))))
 '(dired-flagged ((t (:foreground "#FF0" :background "#700" :bold t))))

 '(fringe ((t (:background "#222222" :foreground "#BBBBBB"))))
 ;; (fringe ((t (:inherit default))))
 '(header-line ((t (:background "#dddddd" :foreground "#888"))))
 '(highlight ((t (:background "#dddddd" :foreground "light blue"))))
 '(holiday-face ((t (:background "#000" :foreground "#777"))))
 '(isearch ((t (:background "#ffbb00" :foreground "#ffffff"))))
 '(isearch-lazy-highlight-face ((t (:background "#ff99bb" :foreground "#ffffff"))))
 ;; (isearch-secondary ((t (:foreground "green"))))
 '(italic ((t (:bold t))))
 '(menu ((t (:background "#000" :foreground "#888"))))
 '(scroll-bar ((t (:background "#222" :foreground "#888"))))
 '(minibuffer-prompt ((t (:foreground "#9F9"))))
 '(modeline ((t (:background "#444" :foreground "#999" :box (:line-width 1 :color "#555")))))
 '(mode-line-inactive ((t (:inherit modeline :background "#2a2a2a" :foreground "#666"))))
 '(modeline-buffer-id ((t (:inherit modeline :foreground "#FF0" :weight bold))))
 '(modeline-mousable ((t (:background "#000" :foreground "#444"))))
 '(modeline-mousable-minor-mode ((t (:background "#ffffff" :foreground "#888"))))
 '(region ((t (:background "#000000"))))
 '(secondary-selection ((t (:background "#aaccff" :foreground "#0088ff"))))
 '(show-paren-match-face ((t (:background "#005500" :foreground "#FFFFFF"))))
 '(show-paren-mismatch-face ((t (:background "Red" :foreground "White"))))
 '(tool-bar ((t (:background "#111" :foreground "#777"))))
 '(tooltip ((t (:background "#333" :foreground "#777"))))
 '(variable-pitch ((t (nil))))
 '(widget-button-face ((t (:bold t :foreground "#888"))))
 '(widget-field-face ((t (:bold t :foreground "#999")))))

(provide-theme 'fruitsalad-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; fruitsalad-dark-theme.el ends here
