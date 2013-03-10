(ns rose.sql-cmd-query
  (require [rose.clu :as clu]
           [clojure.string :as string]))

(defn run [& args]
  (let [result (apply clu/sh args)]
    (when-let [result-out (:out result)]
      (print result-out))
    (when-let [result-err (:err result)]
      (print (clu/ascii-color :red result-err)))
    (println)))

(defn- escape-single-quote [^String s]
  (clojure.string/escape s {\' "\\'"}))

(defn- escape-dash [^String s]
  (clojure.string/escape s {\- "_"}))

(defn to-sql [x]
  (cond
   (string? x) (str \' (escape-single-quote x) \')
   (keyword? x) (escape-dash (name x))
   (vector? x) (let [[f & args] x]
                 (str \(
                      (string/join (str \space (to-sql f) \space)
                                   (map to-sql args))
                      \)))
   :else x))

(defn query [cmd q]
  (run
   (name cmd)
   :< (str (string/join " " (map to-sql q)) \;)))
