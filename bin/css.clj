#!/usr/bin/env clove
;; | clojure

(ns bin.css
  (:require [rose.file :as rfile]
            [rose.clu :as clu]
            [rose.ss-css :reload true]
            [rose.ang-html]
            [clojure.string :as string]))

(defn gen {:cli {:file String}}
  []
  (if-let [input-filename (clu/*opts* :file)]
    (let [output-filename (str (string/replace input-filename
                                               #"\.clj$" "")
                               ".css")]
      (spit (rfile/path output-filename)
            (with-out-str
              (rose.ss-css/print-tree
               (read-string
                (str \[
                     (slurp (rfile/path input-filename))
                     \]))))))
    (rose.ss-css/print-tree
     (read-string (str \[ (slurp *in*) \])))))

(defn colors-sample {:cli {:file String}}
  []
  (spit (rfile/path (rose.clu/*opts* :file))
        (with-out-str

          (rose.ang-html/print-tree
           [:html
            [:head nil]
            [:body
             (for [[color hue-sat] (sort rose.ss-css/known-colors-hue-sat)]
               [:div
                [:span {:style
                        (str "display:inline-block;"
                             "width: 10em; "
                             "background: "
                             (rose.ss-css/color-str hue-sat 0.5 nil))}
                 color]
                [:span {:style
                        (str "display:inline-block;"
                             "width: 10em; "
                             "background: " color)}
                 color]])]]))))

(defn rgb-to-hsl {:cli {}}
  [^String rgbs]
  (doseq [rgb (clojure.string/split rgbs #"\s")]
    (println rgb "->" (vec (map double (rose.ss-css/rgbhexstr-to-hsl rgb))))))

(clu/run-command-maybe-ns *ns* "css.clj")
