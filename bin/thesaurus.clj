#!/usr/bin/env clove
;; | clojure

(ns bin.thesaurus
  (:require [clojure.java.io :as io]
	    [rose.complete]
	    [rose.clu]
	    [rose.thesaurus])
  (:use	[rose.file :only [path]]
	[rose.utils :only [re-parser]]))

(def thesaurus-file 
     (case (System/getProperty "os.name")
       "Mac OS X" (path :home ".local" "share/dictd/moby-thesaurus.index")
       "Linux"    (path "/usr" "share/dictd/moby-thesaurus.index")))

(defprotocol cache-protocol
  (update [this] "update cache")
  (contents [this] "get cache"))

(def parse-line
     (re-parser #"(.*)\s+(\S+)\s+(\S+)"
		[nil :word :ind1 :ind2]))

(defrecord cache-mech [content age]
  cache-protocol
  (update [this]
  	  (let [content (:content this)
  		age (:age this)]
	    (dosync
	     (ref-set content
		      (map #(:word (parse-line %))
			   (line-seq (io/reader thesaurus-file))))
	     
	     (reset! age
		     (.lastModified (java.io.File. thesaurus-file))))))
  (contents [this]
	    (when (> (.lastModified (java.io.File. thesaurus-file))
		     @(:age this))
	      (.update this))
	    @(:content this)))

(def thesaurus-cache (cache-mech. (ref []) (atom 0)))

(defrecord word-class [nom]
  rose.complete/compable
  (complete [this] 
	    (filter #(.startsWith (.toLowerCase %)
				  (.toLowerCase (:nom this)))
		    (.contents thesaurus-cache))))

(defn print-vector [v]
  (let [L 80 ;; columns
	n (count v)
	x (reduce max 0 (map count v))
	c (long (/ (inc L) (inc x)))
	r (long (Math/ceil (/ n c)))]
    (when (not-empty v)
      (doseq [line (apply map vector (partition
				      r r (repeat nil)
				      (sort (seq v))))]
	(apply println (map #(format (str "%-" x "s") %)
			    (filter identity line)))))))

(defn search {:doc "search thesaurus"
	      :cli-default true
	      :cli {}}
  [^word-class & words]
  (print-vector (apply rose.thesaurus/thesaurus words)))

;; the type hint is placed on '&, by which we mean all the following
;; arguments have this type.

;; The completion is not good enough. Ideally it should also depend on
;; prior arguments.

(rose.clu/run-command-maybe)
