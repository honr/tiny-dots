(defproject jar-store "0.0.0-SNAPSHOT"
  :description "virtual package for obtaining all dependencies"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/data.json "0.1.3"]
                 [org.clojure/data.csv "0.1.2"]
                 [org.clojure/java.jdbc "0.1.1"]
		 ;; [swank-clojure "1.3.0-SNAPSHOT"]
		 [clj-http "0.4.0"]
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
                 [com.google.guava/guava "12.0"]
		 ;; [javax.mail/mail "1.4.5"]
		 ;; [org.mortbay.jetty/jetty "6.1.15"]
                 ;; ;; jetty moved to eclipse. look for jetty 8.
                 ;; [postgresql "9.1-901.jdbc4"]
		 [xpp3 "1.1.4c"]
		 ;; [compojure "1.0.2"]
		 ;; ;; [hiccup "0.4.0-SNAPSHOT"]
                 ;; ;; [ring "0.3.8"]
                 ;; [ring/ring-jetty-adapter "1.1.0"]
                 ;; [ring/ring-servlet "1.1.0"]
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
