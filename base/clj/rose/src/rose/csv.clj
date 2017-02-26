;; (remove-ns 'rose.csv)
(ns rose.csv
  (:refer-clojure :exclude [slurp spit]) ;; don't :use this ns.
  (:require [clojure.java.io :as io]
	    [clojure.string :as string]))

(defn slurp [filename]
  (let [file-raw (map #(read-string (str "[" % "]"))
		      (line-seq (io/reader filename)))
	header (for [x (first file-raw)]
		 (if (.startsWith x ":")
		   (keyword (.substring x 1))
		   x))
	strct (apply create-struct header)
	body (next file-raw)]
    (for [line body]
      (apply struct strct line))))

(defn spit [filename coll]
  (if (map? (first coll))
    (spit filename
	  (cons (keys (first coll)) (map vals coll)))
    (clojure.core/spit
     filename
     (string/join "\n"
		  (concat
		   (for [row coll]
		     (string/join ","
				  (map #(format "\"%s\"" %) row))))))))
