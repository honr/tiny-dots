#!/usr/bin/env clove
;; | clojure

(ns bin.css
  (:require [rose.file :as rfile]
            [rose.clu :as clu]
            [rose.ss-css :reload true]
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

(clu/run-command-maybe-ns *ns* "css.clj")
