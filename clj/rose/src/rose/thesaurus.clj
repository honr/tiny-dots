(ns rose.thesaurus
  (:use [clojure.set])
  (:require [clojure.java.shell :as shell]))

(defn thesaurus 
  ([word]
     (let [ret (shell/sh "dict" "-d" "moby-thesaurus" "-f" word)]
       (if (zero? (:exit ret))
	 (set
	  (map (memfn trim)
	       (.split 
		(apply str 
		       (drop 3 (.split (:out ret) "\n")))
		","))))))
  ([word & words]
     (reduce intersection (thesaurus word) (map thesaurus words))))
