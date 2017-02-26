(ns rose.inkscape
  (:require [clojure.java.shell]
	    [clojure.walk]
	    [rose.xml]
	    [rose.file]))

(defn exportable-objects [input-file]
  (let [coll (ref [])]
    (clojure.walk/prewalk #(if (and (or (= (:tag %) :g)
					(= (:tag %) :rect))
				    (string? (:id (:attrs %)))
				    (.endsWith (:id (:attrs %)) ".png"))
			     (do (dosync (alter coll conj (:id (:attrs %)))) nil)
			     %)
			  (rose.xml/m<-s (slurp input-file)))
    @coll))


(defn export-png [input-file export-id export-png]
  (clojure.java.shell/sh "inkscape"
			 "--export-id" export-id
			 "--export-png" export-png
			 "--file" input-file))

(defn export-pngs [input-file output-path]
  (clojure.java.shell/sh
   "inkscape" "--shell"
   :in
   (with-out-str
     (doseq [img (exportable-objects input-file)]
       (println input-file
		"--export-id" img
		"--export-png" (rose.file/path output-path img))))))

(defn -main [input-file & args]
  (if args
    (let [output-path (rose.file/path (first args))]
      (rose.file/make-sure-directory-exists output-path)
      (println
       (:out
	(export-pngs (rose.file/path input-file)
		     output-path))))))
