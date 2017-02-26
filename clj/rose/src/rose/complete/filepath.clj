(ns rose.complete.filepath
  (require [rose.file]
	   [rose.complete])
  (:import [java.io File]))

(defn- file-path-list [prefix1 prefix2 name-part]
  ;; TODO: check if it is a directory
  (map
   (if (not-empty prefix2)
     #(str prefix2 "/" %)
     (if prefix1
       identity
       #(str "/" %)))
   (let [dir (File. (str prefix1 "/" prefix2))]
     (if (empty? name-part)
       (.list dir)
       (filter #(.startsWith % name-part)
               (.list dir))))))

(deftype Filepath [nom]
  rose.complete/compable
  (complete [this]
    ;; TODO: pathname expansion ~user
    (let [cur (.nom this)
          cur (if (.startsWith cur "~/")
                (str *home* (.substring cur 1))
                cur)
          i (.lastIndexOf cur "/")]
      (file-path-list
       (when-not (.startsWith cur "/") *cwd*)
       (if (neg? i) "" (.substring cur 0 i))
       (.substring cur (inc i))))))
