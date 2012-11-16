(ns rose.jsoup)

(defprotocol ExportToClojure
  (clj [x] "Export java class to a clojure style structure."))

(extend-protocol ExportToClojure
  org.jsoup.nodes.Document
  (clj [x] (clj (first (.children x))))

  org.jsoup.nodes.Element
  (clj [x]
    (vec
     (concat [(keyword (.tagName x))]
             (let [attrs (clj (.attributes x))]
               (if (empty? attrs) nil [attrs]))
             (remove nil? (map clj (.childNodes x))))))

  org.jsoup.nodes.Attributes
  (clj [x] (into {} (map clj x)))

  org.jsoup.nodes.Attribute
  (clj [x] [(keyword (.getKey x)) (.getValue x)])

  org.jsoup.nodes.TextNode
  (clj [x] (.text x))

  org.jsoup.nodes.DataNode
  (clj [x] (.getWholeData x))

  org.jsoup.nodes.Comment
  (clj [x] nil))

(defprotocol SelectElements
  (sel [x ^String query] "Elements matching the query."))

(extend-protocol SelectElements
  org.jsoup.nodes.Element
  (sel [x query]
    (org.jsoup.select.Selector/select query x)))
