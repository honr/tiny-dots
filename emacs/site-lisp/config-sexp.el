(require 'paredit)

(defface paren-face
  '((((class color) (background dark)) (:foreground "#648864"))
    (((class color) (background light)) (:foreground "grey60")))
  "Face used to dim parentheses.")

(dolist (mode (list 'lisp-interaction-mode 'emacs-lisp-mode 'lisp-mode))
  (font-lock-add-keywords mode '(("(\\|)" . 'paren-face)) 'append))

(dolist (hook (list 'emacs-lisp-mode-hook 'lisp-mode-hook))
  (add-hook hook (lambda () (paredit-mode +1))))

(require 'clojure-mode)
(add-hook 'clojure-mode-hook
          (lambda ()
            (paredit-mode +1)
            (subword-mode t)))
(font-lock-add-keywords
 'clojure-mode
 '(("(\\|)\\|\\[\\|\\]\\|{\\|}" . 'paren-face)) 'append)

(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'auto-mode-alist '("/AA$" . clojure-mode))

(add-to-list 'auto-insert-alist
             '(clojure-mode
               nil ;; No options.
               "#!/usr/bin/env clove" ?\n
               ";; | clojure" ?\n
               ?\n
               "(ns bin.hello" ?\n
               "  (:require [rose.file :as rfile]" ?\n
               "            [rose.clu :as clu]" ?\n
               "            [rose.xml :as xml]" ?\n
               "            [rose.gplot :as gplot])" ?\n
               "  (:import [java.io File]" ?\n
               "           [org.joda.time" ?\n
               "            DateTime Months Weeks MutableDateTime]))" ?\n
               ?\n
               "(defn main {:cli {:v Boolean}} []" ?\n
               "  \"Greets or prints the version.\"" ?\n
               "  (if (clu/*opts* :v)" ?\n
               "    (println \"Version: 0.1\")" ?\n
               "    (println (format \"Greetings, %s.\" *username*))))" ?\n
               ?\n
               "(clu/run-command-maybe-ns *ns* \"hello.clj\")"))

(setq nxml-sexp-element-flag t)

(require 'lib-sexp)

(provide 'config-sexp)
