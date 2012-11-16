(ns rose.xml
  (:require [clojure.pprint]
	    ;; [clojure.contrib prxml]
	    [rose.prxml]
	    ;; [clojure.contrib.lazy-xml :as lazy-xml]
            ;; vanished! cloning here.
            [rose.xml.lazy-xml :as lazy-xml]
	    [clojure.xml])
  (:use [rose.utils :only [unshy]])
  (:import [org.xml.sax InputSource]
	   [java.io Reader]))

(def default-parser lazy-xml/startparse-sax)

(try
  (load "xml/tagsoup")
  (catch Exception e
    (when-not (re-find #"tagsoup" (str e))
      (throw e))))

(def queue-size Integer/MAX_VALUE)

(defn m<-s
  ([the-parser s]
     (lazy-xml/parse-trim (java.io.StringReader. s)
			  the-parser queue-size))
  ([s]
     (lazy-xml/parse-trim (java.io.StringReader. s))))

(defn v<-m [tree]
  (if-let [tag (:tag tree)]
    `[~tag
      ~@(let [attrs (:attrs tree)] (when (not-empty attrs) (list attrs)))
      ~@(when-let [content (:content tree)]
	  (map v<-m content))]
    tree))

(defn v<-s [& args]
  (v<-m (apply m<-s args)))

(defn s<-v [tree]
  (with-out-str
    (rose.prxml/prxml tree)))

(defn s<-m [tree]
  (with-out-str
    (clojure.xml/emit tree))) ;; test this one.

(defn files-v<-s [input-str-file output-vector-file]
  (spit
   output-vector-file
   (with-out-str
     (clojure.pprint/pprint
      (v<-s (slurp input-str-file))))))

(defn files-s<-v [input-vector-file output-str-file]
  (spit
   output-str-file
   (str "<?xml version=\"1.0\"?>"
	(s<-v (read-string (slurp input-vector-file))))))


;; selector

(defmulti sel-process (fn [f x] (class f)))
(prefer-method sel-process clojure.lang.IPersistentMap clojure.lang.IFn)
(prefer-method sel-process clojure.lang.IPersistentVector clojure.lang.IFn)

(def match-helper-map
     {:t [:tag]
      :i [:attrs :id]
      :c [:attrs :id]})

(defn match-map [m x]
  (every? identity
	  (for [[k v] m]
	    (= v
	       (if-let [ks (match-helper-map k)]
		 (get-in x ks)
		 (get (get x :attrs) k))))))

;; (defmethod sel-process clojure.lang.IPersistentMap [f x]
;;   ((if (:list f) #(map :content %) #(:content (first %)))
;;    (filter #(match-map (dissoc f :list) %) x)))

(defmethod sel-process clojure.lang.IPersistentMap [f x]
  (:content
   (first
    (filter #(match-map f %) x))))

(defmethod sel-process clojure.lang.Keyword [f x]
  (:content
   (first
    (filter #(= f (get % :tag)) x))))

(defmethod sel-process String [f x]
  (loop [f f x x]
    (if (empty? f)
      x
     (let [[selector-str tag-part class-part id-part residue]
	   (re-matches #"([^ .#]*)?(?:\.([^ .#]*))?(?:#([^ .#]*))?(?: (.*))?"
		       f)]
       (recur
	residue
	(:content
	 (first
	  (filter (fn [y]
		    (and (or (empty? tag-part) (= tag-part (name (get y :tag))))
			 (or (empty? class-part) (= class-part (get-in y [:attrs :class])))
			 (or (empty? id-part) (= id-part (get-in y [:attrs :id])))))
		  x))))))))

;; (sel [(m<-s "<a id=\"foo\"><b>c</b></a>")] "a#foo b")

(defmethod sel-process Integer [f x]
  (:content
   (cond (= f 0) (first x)
	 (= f 1) (second x)
	 true    (nth x f))))

(defmethod sel-process Long [f x]
  (:content
   (cond (= f 0) (first x)
	 (= f 1) (second x)
	 true    (nth x f))))

(defmethod sel-process clojure.lang.IPersistentVector [f x]
  (map #(reduce (fn [a b] (sel-process b a)) [%] f) x))

(defmethod sel-process clojure.lang.IFn [f x]
  (map f x))

(defn sel [x & forms]
  (reduce (fn [a b] (sel-process b a)) x forms))

(comment

  (def sample-xml-strings
       ["<a>b</a>"
	"<a></a>"
	"<a href=\"s\">b</a>"
	"<a/>"])
  (m<-s (first sample-xml-strings))
  (m<-s lazy-xml/startparse-sax (first sample-xml-strings))
  (m<-s lazy-xml/startparse-tagsoup (first sample-xml-strings))

  (lazy-xml/parse-trim (java.io.StringReader. "<a>b</a>"))


  (map m<-s sample-xml-strings)
  ({:tag :a, :attrs {}, :content ("b")}
   {:tag :a, :attrs {}, :content ()}
   {:tag :a, :attrs {:href "s"}, :content ("b")}
   {:tag :a, :attrs {}, :content ()})

  (sel [{:tag :html, :attrs {},
	 :content (list {:tag :body, :attrs {},
			 :content (list {:tag :a, :attrs {:shape "rect"},
					 :content (list "b" "c")}
					{:tag :a, :attrs {:shape "rect"},
					 :content (list "d" "e")})})}]
       :html :body [:a])

  (map #(v<-s lazy-xml/startparse-sax %) sample-xml-strings)
  (map v<-s sample-xml-strings)
  ([:a "b"] [:a] [:a {:href "s"} "b"] [:a])

  )

(defn contentize [x]
  (if (map? x)
    (let [h (get-in x [:attrs :href])
	  c (get x :content)
	  r (unshy (filter not-empty (map contentize c)))]
      (unshy (filter not-empty [h r])))
    x))

;; (def *use-tagsoup* false)
;; (def *use-tagsoup* true)

;; (defn m<-s [s]
;;   (if *use-tagsoup*
;;     (clojure.contrib.lazy-xml/parse-trim (java.io.StringReader. s) startparse-tagsoup 1)
;;     (clojure.contrib.lazy-xml/parse-trim (java.io.StringReader. s))))
