#!/usr/bin/env clove
;; -*- Mode: Clojure -*- | clojure

(ns bin.ruler
  (:refer-clojure :exclude [test])
  (:require [rose.web.static-build]
            [rose.clu]
            [rose.file]
            [clojure.java.io :as jio]))

(defn read-rules []
  (let [rule-file (str *cwd* "/rule.clj")]
    (when (rose.file/exists? [rule-file])
      (let [rule-ns-stub (read
                          (clojure.lang.LineNumberingPushbackReader.
                           (jio/reader rule-file)))
            rule-ns (symbol (second rule-ns-stub))]
        (require [rule-ns :reload true])
        {:ns rule-ns
         :publics (ns-publics rule-ns)}))))

(defn filter-prefix [prefix coll]
  (filter #(.startsWith % prefix) coll))

(deftype RulerBuildTarget [nom]
  rose.complete/compable
  (complete [this]
    (let [cur (.nom this)]
      (filter-prefix cur (map #(str (first %))
                              (:publics (read-rules)))))))

(defn ^{:cli {}} build [^RulerBuildTarget targ & args]
  (let [rules-ns (read-rules)]
    (apply (ns-resolve (:ns rules-ns) (symbol targ)) args)))

(defn ^{:cli {}} test [& args]
  (println "... going to test ..."))

(rose.clu/run-command-maybe-ns *ns* "ruler.clj")

;; Needs its own rule completion:
;;   look for a file named RULE.clj in current dir, or limited-parent
;;   (Note: limited-parent: if we are under *home*, go up until we
;;   reach *home*.  otherwise go up until / ).  Once we get the
;;   RULE.clj, load it as a library.  (Note: We need to carefully
;;   control the loading, caching and reloading of libraries.  We
;;   might need to sandbox loadings and runs, together with garbage
;;   collected classes). Look for targets in the loaded ns.  [Optional
;;   Design Decision] Each target is a cli command itself.  Targets
;;   are expected to be hand-sorted to not depend on non-yet-defined
;;   targets.
