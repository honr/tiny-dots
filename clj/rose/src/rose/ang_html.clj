(ns rose.ang-html
  (:require [clojure.string]))

(defn- escape [s]
  (clojure.string/escape s {\< "&lt;"
                            \> "&gt;"
                            \& "&amp;"
                            \' "&apos;"
                            \" "&quot;"}))

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

;; TODO: this should be extensible by the library user.
(defn- eval-fn-node
  ([x]
     (cond
      (list? x) (eval-fn-node (first x) (rest x))
      (vector? x) (with-out-str (print-node x))
      (symbol? x) (js-uglify-name (name x))
      (keyword? x) (name x)
      :else (str x)))
  ([tag args]
     (condp = tag
       'call ;; Js call f(x, y, z).
       (let [[fn-name & fn-args] (map eval-fn-node args)]
         (str fn-name \( (clojure.string/join "," fn-args) \)))

       'scripts
       (str-tree
        (for [script args]
          [:script {:src script} nil]))

       'css
       (str-tree
        (for [stylesheet args]
          [:link {:rel "stylesheet" :href stylesheet}]))

       'a ;; Does not ng-evaluate.
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
          [:a {:href href} href]))

       'meta-utf-8
       (str-tree
        [:meta {:charset "UTF8" :http-equiv "content-type" :content "text/html"}])

       'get ;; Angular {{x}}.
       (str "{{" (apply str (map eval-fn-node args)) "}}")

       'not
       (apply str "!" (map eval-fn-node args))

       'str
       (apply str (map eval-fn-node args))

       'iter
       (let [[v coll] args]
         (str (eval-fn-node v) " in " (eval-fn-node coll)))

       'pipe
       (clojure.string/join " | "
                            (map eval-fn-node args))

       'pipe-call
       (clojure.string/join ":" (map eval-fn-node args))

       ;; Default
       "")))

(defn- print-node [tag attributes contents]
  ;; Print a single node.  Calls print-tree to recurse.
  ;; Note: this "function" prints instead of returning.  Wrap in with-out-str
  ;; when necessary.
  (if tag
    (let [tag-name (name tag)]
      (print "<")
      (print tag-name)
      (doseq [[k v] attributes]
        (let [k-str
              ;; Prepend data-ng- to keywords starting with an uppercase,
              ;; and also turns those to lowercase.  Others are just
              ;; `name'ed as is.
              (let [k-name (name k)]
                (if (Character/isUpperCase (first k-name))
                  (str "data-ng-" (.toLowerCase k-name))
                  k-name))]
          (print
           (if v
             (format " %s=\"%s\"" k-str (escape (eval-fn-node v)))
             (format " %s" k-str)))))

      (if (seq contents)
        ;; Not an empty tag.
        (do
          (print ">")
          (doseq [c contents] (print-tree c))
          (print (format "</%s>" tag-name)))
        ;; Empty tag:
        (print "/>")))

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
     (print (eval-fn-node (first x) (rest x)))
     (doseq [c x] (print-tree c)))

   ;; considered an empty node.
   (or (keyword? x) (symbol? x))
   (print-node x {} nil)

   :else
   (print (escape (str x)))))

(defn str-tree [x]
  (with-out-str (print-tree x)))
