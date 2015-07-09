(ns rose.ang-html
  (:require [clojure.string]))

;; Add the following features:
;; 1. dumb indent (break at every tag).
;; 2. polymer specific shorthands:
;;    import: [link {:rel "import" :href imported-entry}]
;;    def-element:
;;      [polymer-element {:name "x" :attributes "attr1 attr2"}
;;        [template (css "x.css")
;;          ...]
;;        (scripts "other.js" "stuff.js" "x.js")]
;;    for or repeat: [template {:repeat "..."}]
;; 3. evaluate arbitrary clojure code.
(defn- escape [s]
  (clojure.string/escape s {\< "&lt;"
                            \> "&gt;"
                            \& "&amp;"
                            \' "&apos;"
                            \" "&quot;"}))
(defn- escape-value [s]
  (clojure.string/escape s {\" "\\\""}))

(declare print-node)
(declare print-tree)
(declare str-tree)

(defn js-uglify-name [^String s]
  (if-not (re-find #"[a-zA-Z]" s)
    s
    (if (and (.startsWith s "+") (.endsWith s "+"))
      ;; constants
      (.replace (.toUpperCase (.substring s 1 (dec (count s)))) \- \_)

      ;; -names-like-this- -> _namesLikeThis_
      (let [[beg coll] (split-with
                        empty?
                        (loop [p s beg [] coll []]
                          (let [n (.indexOf p "-")]
                            (if (neg? n)
                              (conj coll p)
                              (recur (.substring p (inc n))
                                     beg
                                     (conj coll (.substring p 0 n)))))))]
        (apply str
               (apply str (repeat (count beg) \_))
               (first coll)
               (map (fn [^String x]
                      (if (empty? x)
                        \_
                        (str (.toUpperCase (.substring x 0 1))
                             (.substring x 1))))
                    (rest coll)))))))

(def shortenable-tags-set
  #{"br" "link" "hr" "img" "meta"})

(defmulti process-node (fn [tag _] tag))

(defn- process-argless-node [x]
  (cond
   (list? x) (process-node (first x) (rest x))
   (vector? x) (with-out-str (print-node x))
   (symbol? x) (js-uglify-name (name x))
   (keyword? x) (name x)
   :else (str x)))

(defmethod process-node 'call [_ args]
  ;; Javascript call f(x, y, z).
  (let [[fn-name & fn-args] (map process-argless-node args)]
    (str fn-name \( (clojure.string/join "," fn-args) \))))

(defmethod process-node 'scripts [_ args]
  (str-tree
   (for [script args]
     [:script {:src script} nil])))

(defmethod process-node 'import [_ args]
  (str-tree
   (for [imported-entry args]
     [:link {:rel "import" :href imported-entry}])))

(defmethod process-node 'css [_ args]
  (str-tree
   (for [stylesheet args]
     [:link {:rel "stylesheet" :href stylesheet}])))

(defmethod process-node 'a [_ args]
  ;; Does not ng-evaluate.
  (let [[base-url & [parameters]] args
        href (str
              base-url
              (when parameters
                (str \?
                     (clojure.string/join
                      "&"
                      (for [[k v] parameters]
                        (str (java.net.URLEncoder/encode (name k))
                             \=
                             (java.net.URLEncoder/encode (str v))))))))]
    (str-tree
     [:a {:href href} href])))

(defmethod process-node 'meta-utf-8 [_ args]
  (str-tree
   [:meta {:charset "UTF8" :http-equiv "content-type" :content "text/html"}]))

(defmethod process-node 'get [_ args]
  ;; Angular {{x}}.
  (str "{{" (apply str (map process-argless-node args)) "}}"))

(defmethod process-node 'not [_ args]
  (apply str "!" (map process-argless-node args)))

(defmethod process-node 'str [_ args]
  (apply str (map process-argless-node args)))

(defmethod process-node 'iter [_ args]
  (let [[v coll] args]
    (str (process-argless-node v) " in " (process-argless-node coll))))

(defmethod process-node 'pipe [_ args]
  (clojure.string/join " | "
                       (map process-argless-node args)))

(defmethod process-node 'pipe-call [_ args]
  (clojure.string/join ":" (map process-argless-node args)))

(defmethod process-node :default [_ args]
  "") ;; Perhaps we should emit an error?

;; TODO: Preprocess nodes to end up with a vector of strings.  Do the printing
;; in another step.
(defn- print-node [tag attributes contents]
  ;; Print a single node.  Calls print-tree to recurse.
  ;; Note: this "function" prints instead of returning.  Wrap in with-out-str
  ;; when necessary.
  (if tag
    (let [tag-name (name tag)]
      (print "<")
      (print tag-name)
      (doseq [[k v] (sort attributes)]
        (let [k-str (name k)]
         (print
          (if v
            (format " %s=\"%s\"" k-str (escape-value (process-argless-node v)))
            (format " %s" k-str)))))

      (if (or (seq contents)            ; Not an empty tag.
              (not (shortenable-tags-set tag-name)))
        (do
          (print ">")
          (doseq [c contents] (print-tree c))
          (print (format "</%s>" tag-name)))
        (print "/>")))                  ; Using short from for empty tags.

    ;; Tag is nil.  Print raw contents.
    (doseq [c contents] (print c))))

(defn print-tree [x]
  "Print HTML to *out*.  Vectors become XML tags: the first item is the
  tag name; optional second item is a map of attributes.
  Sequences are processed recursively.

    (print-tree [:p {:class \"greet\"} [:i \"Ladies & gentlemen\"]])
    ; => <p class=\"greet\"><i>Ladies &amp; gentlemen</i></p>

  To insert raw strings, use `nil' as the tag:
    (print-tree [:p [nil \"<i>here & gone</i>\"]])
    ; => <p><i>here & gone</i></p>"
  ;; Calls print-node for each node.
  (cond
   (vector? x)
   (let [[tag & body] x]
     (if (map? (first body))
       (print-node tag (first body) (rest body))
       (print-node tag {} body)))

   (seq? x)
   (if (symbol? (first x))
     (print (process-node (first x) (rest x)))
     (doseq [c x] (print-tree c)))

   ;; considered an empty node.
   (or (keyword? x) (symbol? x))
   (print-node x {} nil)

   :else
   (print (escape (str x)))))

(defn str-tree [x]
  (with-out-str (print-tree x)))
