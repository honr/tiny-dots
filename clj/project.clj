(defproject jar-store "0.0.0-SNAPSHOT"
  :description "virtual package for obtaining all dependencies"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/data.csv "0.1.3"]
                 [org.clojure/java.jdbc "0.6.1"]
                 [org.clojure/core.async "0.2.385"]
                 [org.clojure/core.cache "0.6.5"]
                 [org.clojure/core.logic "0.8.10"]
                 [org.clojure/core.memoize "0.5.9"]
                 [org.clojure/core.typed "0.3.19"]
                 [org.clojure/tools.cli "0.3.5"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.clojure/tools.nrepl "0.2.12"]
                 [org.clojure/tools.reader "0.10.0"]
                 ;; [org.clojure/core.unify "0.5.6"]
                 ;; [org.clojure/core.match "0.2.1"]
                 ;; [swank-clojure "1.4.2"]
                 [org.mozilla/rhino "1.7.7.1"]

                 [com.google.javascript/closure-compiler "v20160713"
                  ;; "v20160315r" ;; apprently this one worked.
                  ;; "v20160517" "v20160619"
                  ]
                 [org.clojure/clojurescript "1.9.198"]
                 [clj-http "3.1.0"]
                 ;;;; FROM HERE:
                 ;; [org.apache.httpcomponents/httpclient "4.1.2"]
                 ;; [incanter/incanter-core "1.5.1"]
                 ;; [incanter/incanter-charts "1.5.1"]
                 ;; ;; [congomongo "0.1.3-SNAPSHOT"]
                 ;; [clj-stacktrace "0.2.4"]
                 ;; [commons-codec "1.5"]
                 ;; [colt "1.2.0"]
                 ;; [commons-fileupload "1.2.2"]
                 ;; ;; [commons-fileupload "1.2.1" :classifier "javadoc"]
                 ;; [commons-io "2.0.1"] ;; 1.4
                 [com.google.guava/guava "19.0"]
                 [javax.mail/mail "1.4.7"]
                 ;; [org.mortbay.jetty/jetty "6.1.15"]
                 [org.eclipse.jetty/jetty-server "9.3.11.v20160721"]
                 ;; [postgresql "9.1-901.jdbc4"]
                 [xpp3 "1.1.4c"]
                 [org.jsoup/jsoup "1.9.2"]
                 [compojure "1.5.1"]
                 [hiccup "1.0.5"]
                 ;; ;; [ring "0.3.8"]
                 [ring/ring-jetty-adapter "1.5.0"]
                 [ring/ring-servlet "1.5.0"]
                 ;; [clout "1.0.1"]
                 [org.yaml/snakeyaml "1.17"]
                 ;;;; UP TO HERE.
                 ;; [jgraph "5.13.0.0"]
                 ;; [lwjgl "2.2.2"]
                 [joda-time "2.9.4"]
                 [org.xerial/sqlite-jdbc "3.8.11.2"]
                 [com.cognitect/transit-clj "0.8.288"]
                 [com.cognitect/transit-js "0.8.846"]
                 ;; [tagsoup "1.2"]
                 ;; [org.apache.maven/maven-ant-tasks "2.1.3"]
                 ;; [org.apache.ant/ant "1.8.2"]
                 ;; [robert/hooke "1.1.3"]
                 [javax.websocket/javax.websocket-api "1.1"]]

  :library-path "lib")

;; http://clojars.org/repo/
;; http://repo2.maven.org/maven2/
