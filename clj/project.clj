(defproject jar-store "0.0.0-SNAPSHOT"
  :description "virtual package for obtaining all dependencies"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/data.json "0.1.3"]
                 [org.clojure/data.csv "0.1.2"]
                 [org.clojure/java.jdbc "0.1.1"]
                 ;; [swank-clojure "1.4.2"]
                 [org.mozilla/rhino "1.7R4"]

                 ;; For ClojsureScript:
                 ;; [com.google.javascript/closure-compiler "r2180"]
                 ;; [org.clojure/google-closure-library "0.0-2029"]
                 [org.clojure/clojurescript "0.0-1450"]

                 [clj-http "0.5.6"]
                 ;;;; FROM HERE:
                 ;; [org.apache.httpcomponents/httpclient "4.1.2"]
                 ;; [incanter "1.3.0"]
                 ;; ;; [congomongo "0.1.3-SNAPSHOT"]
                 ;; [clj-stacktrace "0.2.4"]
                 ;; [commons-codec "1.5"]
                 ;; [colt "1.2.0"]
                 ;; [commons-fileupload "1.2.2"]
                 ;; ;; [commons-fileupload "1.2.1" :classifier "javadoc"]
                 ;; [commons-io "2.0.1"] ;; 1.4
                 [com.google.guava/guava "13.0.1"]
                 ;; [javax.mail/mail "1.4.5"]
                 ;; [org.mortbay.jetty/jetty "6.1.15"]
                 ;; ;; jetty moved to eclipse. look for jetty 8.
                 ;; [postgresql "9.1-901.jdbc4"]
                 [xpp3 "1.1.4c"]
                 [org.jsoup/jsoup "1.6.3"]
                 [compojure "1.1.3"]
                 [hiccup "1.0.1"]
                 ;; ;; [ring "0.3.8"]
                 [ring/ring-jetty-adapter "1.1.6"]
                 [ring/ring-servlet "1.1.6"]
                 ;; [clout "1.0.1"]
                 ;;;; UP TO HERE.
                 ;; [jgraph "5.13.0.0"]
                 ;; [lwjgl "2.2.2"]
                 [joda-time "2.1"]
                 [org.xerial/sqlite-jdbc "3.7.2"]
                 ;; [tagsoup "1.2"]
                 ;; [org.apache.maven/maven-ant-tasks "2.1.3"]
                 ;; [org.apache.ant/ant "1.8.2"]
                 ;; [robert/hooke "1.1.3"]
                 ]

  :library-path "lib")

;; http://clojars.org/repo/
;; http://repo2.maven.org/maven2/
