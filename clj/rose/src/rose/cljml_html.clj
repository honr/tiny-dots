(ns rose.cljml-html
  (:require [clojure.string]))

(declare print-tree)

(defn- escape [s]
  (clojure.string/escape s {\< "&lt;"
                            \> "&gt;"
                            \& "&amp;"
                            \' "&apos;"
                            \" "&quot;"}))

(defn- print-tag [tag attributes contents]
  (if tag
    (let [tag-name (name tag)]
      (print "<")
      (print tag-name)
      (doseq [[k v] attributes]
        (print (format " %s=\"%s\"" (name k) (escape (str v)))))
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

  (condp instance? x

    clojure.lang.IPersistentVector (let [[tag & body] x]
                                     (if (map? (first body))
                                       (print-tag tag (first body) (rest body))
                                       (print-tag tag {} body)))

    clojure.lang.ISeq (doseq [c x] (print-tree c))

    clojure.lang.Keyword (print-tag x {} nil)

    String (print (escape x))

    ;; Default.
    (when x
      (print x))))
