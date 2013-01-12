#!/usr/bin/env clove
;; | clojure

(ns bin.url-decode
  (:require [rose.file :as rfile]
            [rose.clu :as clu]
            [rose.xml :as xml]
            [clojure.string :as string])
  (:import [java.io File]
           [java.net URLDecoder URLEncoder]))

(def dangerous-characters #"[/:\n\t\f]")

(defn decode {:cli {} :cli-default true}
  [& input-filenames]
  "Fixes url encoded filenames"
  (doseq [input-filename input-filenames]
    (let [decoded-filename (string/replace
                            (URLDecoder/decode input-filename)
                            dangerous-characters " ")]
      (when-not (= input-filename decoded-filename)
        (println decoded-filename)
        (.renameTo (File. (rfile/path input-filename))
                   (File. (rfile/path decoded-filename)))))))

(clu/run-command-maybe-ns *ns* "url_decode.clj")
