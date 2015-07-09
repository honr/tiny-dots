#!/usr/bin/env clove
;; | clojure

(ns bin.ang
  (:require [rose.file :as rfile]
            [rose.clu :as clu]
            [rose.ang-html :reload true]
            [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.string :as string]))

(defn gen {:cli {:file String}}
  []
  (if-let [input-filename (clu/*opts* :file)]
    (let [output-filename (str (string/replace input-filename
                                               #"\.clj$" "")
                               ".html")]
      (spit (rfile/path output-filename)
            (with-out-str
              (rose.ang-html/print-tree
               (read-string
                (slurp (rfile/path input-filename)))))))
    (rose.ang-html/print-tree
     (read-string (slurp *in*)))))

(clu/run-command-maybe-ns *ns* "ang.clj")
