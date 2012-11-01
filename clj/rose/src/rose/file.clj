(ns rose.file
  (:import [java.io File]
	   [java.net URI URLEncoder]))

;; some of the functions are imported from lib.sfd.file-utils:
;; git://github.com/francoisdevlin/devlinsf-clojure-utils.git

(defn path [beg & colls]
  "Helper for creating path strings. Works as following examples show:
\"/foo\" -> \"/foo\"
\"foo\" -> \"$PWD/foo\"
When *cwd* is not available, uses System/getProperty \"user.dir\" instead.
"
  (apply str
	 (interpose
	  "/"
	  (conj colls
		(if (and (string? beg)
                         (not (.startsWith beg "/")))
                  (str (or *cwd* (System/getProperty "user.dir")) "/" beg)
                  beg)))))

(defmulti fpath class)
(defmethod fpath File [f] f)
(defmethod fpath String [s] (File. s))
(defmethod fpath clojure.lang.PersistentVector [v]
  (File. (apply path v)))

(defn fpath* [& args]
  (File. (apply path args)))

(defn ls
  ([]
     (ls [*cwd*]))
  ([f]
     (seq (.list (fpath f))))
  ([f pattern]
     (filter #(re-find pattern %) (ls f))))

(defn ls-abs [fs & args]
  (mapcat 
   (fn [f] (map #(conj f %) (apply ls f args)))
   fs))

(defn directory? [f] (.isDirectory (fpath f)))
(defn exists? [f] (.exists (fpath f)))
(defn file? [f] (.isFile (fpath f)))
(defn hidden? [f] (.isHidden (fpath f)))
(defn readable? [f] (.canRead (fpath f)))
(defn writable? [f] (.canWrite (fpath f)))
(defn last-modified [f] (.lastModified (fpath f)))
(defn length [f] (.length (fpath f)))

(defn touch [f] (.createNewFile (fpath f)))
(defn mkdir [f]
  "Creates dir and its parents."
  (.mkdirs (fpath f)))
(defn mv [source dest] (.renameTo (fpath source) (fpath dest)))

(defn cp [source-path dest-path]
  (let [dest-file (java.io.File. dest-path)]
    (when (.exists dest-file)
      (.createNewFile dest-file)))
  (with-open [source-fd (.getChannel (java.io.FileInputStream. source-path))
              dest-fd (.getChannel (java.io.FileOutputStream. dest-path))]
    (.transferFrom dest-fd source-fd 0 (.size source-fd))))

(defn rm [f] (.delete (fpath f)))

(defn head
  ([f]
     (first (line-seq (clojure.java.io/reader (fpath f)))))
  ([n f]
     (take n (line-seq (clojure.java.io/reader (fpath f))))))

(defn make-sure-directory-exists [p]
  (let [d (fpath p)] (when-not (.exists d) (.mkdirs d))))

(defn make-sure-parent-directory-exists [p]
  (let [d (File. (.getParent (fpath p)))] (when-not (.exists d) (.mkdirs d))))

(defn path-mspde [& args]
  "same as path, except makes sure parent directory exists."
  (let [p (apply path args)]
    (make-sure-parent-directory-exists p)
    p))

(defn make-sure-directory-is-empty [p]
  "Renames the directory to *-old. beware if *-old already exists it will be modified."
  (let [d (fpath p)]
    (when (.exists d)
      (.renameTo d (fpath (str (fpath p) "-old"))))
    ;; TODO delete it afterwards.
    (.mkdirs d)))

(defn browse [s] (.browse (java.awt.Desktop/getDesktop) (URI. s)))

(defn download-file [f url]
 (.transferFrom 
  (.getChannel 
   (java.io.FileOutputStream. 
    f))
  (java.nio.channels.Channels/newChannel 
   (.openStream
    (java.net.URL. 
     url)))
  0 0x1000000))
