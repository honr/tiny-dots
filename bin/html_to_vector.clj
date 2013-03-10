#!/usr/bin/env clove
;; | clojure

(ns bin.html-to-vector
  (:require [rose.jsoup]
            [rose.clu :as clu]
            [clojure.pprint]
            [clojure.java.io :as io]))

(defn main {:cli {:url String :u :url
                  :query String :e :query
                  :raw Boolean}
            :cli-default true}
  []
  (let [document (if-let [url (clu/*opts* :url)]
                   (org.jsoup.Jsoup/parse
                    (java.net.URL. url)
                    10000) ;; Wait 10 seconds.
                   (org.jsoup.Jsoup/parse
                    (slurp *in*)))
        cljml-document (if-let [query (clu/*opts* :query)]
                         (map rose.jsoup/clj
                              (rose.jsoup/sel document query))
                         (rose.jsoup/clj document))]
    (if (clu/*opts* :raw)
      (println cljml-document)
      (clojure.pprint/pprint
       cljml-document
       *out*))))

(clu/run-command-maybe-ns *ns* "html_to_vector.clj")
