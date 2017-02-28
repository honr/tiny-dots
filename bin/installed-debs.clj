#!/usr/bin/env clove
;; | clojure

(ns bin.installed-debs
  (:require [rose.clu :as clu]
            [clojure.string]))

(def r1 (rose.utils/re-parser
         #"([^ ]*) *([^ ]*) .*"
         [:MATCH :status :pkg]))

(defn retrieve []
  (filter identity
          (map (comp :pkg r1)
               (clojure.string/split-lines
                (:out (rose.clu/sh "dpkg" :l))))))

(defn main {:cli {} :cli-default true} []
  "Greets or prints the version."
  (println (retrieve)))

(clu/run-command-maybe-ns *ns* "installed-debs.clj")
