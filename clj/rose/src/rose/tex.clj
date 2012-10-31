(ns rose.tex
  (:require [clojure.string]
            [rose.file :as rfile]
            [rose.clu :as clu]
            [rose.cljml-tex]))

(def cache-dir (str *home* "/.local/run/texsvg"))
(def ^{:dynamic true} *svg-store-url-prefix* "cache/")

(defn generate-svg [snippet file-out {additional-tex-includes :extra-includes
                                      tmp-base :tmp-base}]
  (let [tmp-file-base (or tmp-base (str *cwd* \/ "__tmp_file"))]
    (spit (str tmp-file-base ".tmp-body")
          snippet)
    (spit (str tmp-file-base ".tex")
          (format (str "\\documentclass[12pt]{article}\n"
                       additional-tex-includes
                       "\\usepackage[active,xetex,tightpage]{preview}\n"
                       "\\begin{document}\n"
                       "\\begin{preview}\n"
                       (rose.cljml-tex/transform snippet)
                       "\n"
                       "\\end{preview}\n"
                       "\\end{document}\n")))
    (clu/sh "xelatex" (str tmp-file-base))
    (clu/sh "pdftocairo" (str tmp-file-base ".pdf") "-svg")
    (rfile/mv (str tmp-file-base ".svg") (rfile/path file-out))
    (rfile/rm (str tmp-file-base ".tex"))
    (rfile/rm (str tmp-file-base ".pdf"))
    ;; (rfile/rm (str tmp-file-base ".log"))
    (rfile/rm (str tmp-file-base ".aux"))))

(defn preprocess-embed-formula [x opts]
  (when-not (.exists (java.io.File. cache-dir))
    (.mkdirs cache-dir))
  (if (not
       (and (instance? clojure.lang.IPersistentVector x)
            (or (= :$ (first x))
                (= :eqn (first x)))))
    x

    (let [body-m (rest x)
          [attr contents] (if (map? (first body-m))
                            [(first body-m) (rest body-m)]
                            [{} body-m])
          contents-str (clojure.string/join "" contents)
          hash-c (hash contents-str)
          cached-svg (format "%012X.svg" hash-c)
          cached-svg-file (str cache-dir \/ cached-svg)]
      (when-not (.exists (java.io.File. cached-svg-file))
        (rfile/make-sure-parent-directory-exists cached-svg-file)
        (generate-svg
         (condp = (first x)
           :$ (format "$%s$" contents-str)
           :eqn (format "\\begin{align*}%s\\end{align*}" contents-str))
         cached-svg-file
         opts))
      [:img {:class ({:$ "inline-math"
                      :eqn "display-math"} (first x))
             :src (str *svg-store-url-prefix* cached-svg)
             :alt contents-str}])))
