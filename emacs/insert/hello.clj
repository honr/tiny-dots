#!/usr/bin/env clove
;; | clojure

(ns bin.hello
  (:require [rose.file :as rfile]
            [rose.clu :as clu]
            [rose.xml :as xml]
            [rose.gplot :as gplot])
  (:import [java.io File]
           [org.joda.time
            DateTime Months Weeks MutableDateTime]))

(defn main {:cli {:v Boolean}} []
  "Greets or prints the version."
  (if (clu/*opts* :v)
    (println "Version: 0.1")
    (println (format "Greetings, %s." *username*))))

(clu/run-command-maybe-ns *ns* "hello.clj")
