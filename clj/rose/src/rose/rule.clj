(ns rose.rule
  (:require [rose.file :as rfile]
            [rose.clu :as clu]
            [rose.cljml-tex]
            [rose.cljml-html]
            [rose.tex]
            [clojure.walk :as walk]))

(defn- generate-tex [file-in file-out]
  (spit (rfile/path file-out)
        (rose.cljml-tex/transform
         (read-string (slurp (rfile/path file-in))))))

(defn cljml->tex [target & deps]
  (generate-tex (first deps) target))

(defn xelatex-2 [target & deps]
  (clu/sh "xelatex" (first deps))
  (clu/sh "xelatex" (first deps)))

(defn figure-tex->pdf [target & deps]
  (clu/sh "xelatex"
          (str "\\newcommand{"
               "\\figname}{"
               (first deps)
               "} "
               (format "\\input{%s}" (second deps))))
  (clu/sh "mv" "figureconv.pdf" target)
  (clu/sh "rm" "figureconv.aux" "figureconv.log"))

(defn svg->pdf [target & deps]
  (clu/sh "inkscape" :export-pdf target (first deps)))

(defn- strip-prefix [p]
  (let [n-slash (.lastIndexOf p "/")
        n-period (.lastIndexOf p ".")]
    (if (< n-slash n-period)
      (.substring p 0 n-period)
      p)))

(defn cljml->svg [target & deps]
  (rose.tex/generate-svg
   (read-string (slurp (rfile/path (first deps))))
   target
   {:extra-includes (apply str
                           (for [package (rest deps)]
                             (format "\\usepackage{%s}\n"
                                     (strip-prefix package))))
    :tmp-base (str *cwd* "/__tmp_file")}))

(defn cljml->html [target & deps]
  (let [file-out target
        file-in (first deps)
        opts {:extra-includes (apply str
                                     (for [package (rest deps)]
                                       (format "\\usepackage{%s}\n"
                                               (strip-prefix package))))}]
    (spit (rfile/path file-out)
          (with-out-str
            (print "<?xml version=\"1.0\"?>")
            (rose.cljml-html/print-tree
             (walk/prewalk (fn [x]
                             (rose.tex/preprocess-embed-formula x opts))
                           (read-string (slurp (rfile/path file-in)))))))))
