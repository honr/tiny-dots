(defproject jar-store "0.0.0"
  :description "virtual package for obtaining all dependencies"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/algo.generic "0.1.2"]
                 [org.clojure/algo.monads "0.1.6"]
                 [org.clojure/core.async "0.2.395"]
                 [org.clojure/core.cache "0.6.5"]
                 [org.clojure/core.logic "0.8.11"]
                 ;; [org.clojure/core.match "0.2.2"]
                 [org.clojure/core.memoize "0.5.9"]
                 [org.clojure/core.typed "0.3.28"]
                 ;; [org.clojure/core.unify "0.5.6"]
                 [org.clojure/data.csv "0.1.3"]
                 [org.clojure/data.json "0.2.6"]
                 ;; [org.clojure/data.xml "0.0.8"]
                 [org.clojure/data.zip "0.1.2"]
                 ;; [org.clojure/java.classpath "0.2.3"]
                 [org.clojure/java.jdbc "0.6.1"]
                 [org.clojure/test.check "0.9.0"]
                 [org.clojure/tools.cli "0.3.5"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.clojure/tools.nrepl "0.2.12"]
                 [org.clojure/tools.reader "0.10.0"]
                 ;; [swank-clojure "1.4.2"]
                 [org.mozilla/rhino "1.7.7.1"]

                 ;; v20160315r and then v20160713 has worked before this.
                 [com.google.javascript/closure-compiler "v20161024"]
                 [org.clojure/clojurescript "1.9.293"]
                 [clj-http "3.3.0"]
                 [com.google.guava/guava "19.0"]
                 [com.google.protobuf/protobuf-java "3.1.0"]
                 [javax.mail/mail "1.4.7"]
                 [javax.websocket/javax.websocket-api "1.1"]
                 ;; "9.4.0.RC1" might depend on unix sockets.
                 [org.eclipse.jetty/jetty-server "9.3.11.v20160721"]
                 [org.postgresql/postgresql "9.4.1211"]
                 [xpp3 "1.1.4c"]
                 [org.jsoup/jsoup "1.10.1"]
                 [compojure "1.5.1"]
                 [hiccup "1.0.5"]
                 [ring/ring-jetty-adapter "1.5.0"]
                 [ring/ring-servlet "1.5.0"]
                 ;; [clout "2.1.2"] ; an http route matching library.
                 [org.yaml/snakeyaml "1.17"]
                 [joda-time "2.9.4"]
                 [org.xerial/sqlite-jdbc "3.14.2.1"]
                 [com.cognitect/transit-clj "0.8.290"]
                 [com.cognitect/transit-js "0.8.846"]

                 ;; a bit experimental below here:
                 [io.grpc/grpc-all "1.0.1"]
                 [io.netty/netty-all "4.1.6.Final"]
                 ]
  ;; Current disabled dependencies:
  ;; [jgraph "5.13.0.0"]
  ;; [lwjgl "2.2.2"]
  ;; [tagsoup "1.2"]
  ;; [org.apache.maven/maven-ant-tasks "2.1.3"]
  ;; [org.apache.ant/ant "1.8.2"]
  ;; [robert/hooke "1.1.3"]
  ;; [org.apache.httpcomponents/httpclient "4.1.2"]
  ;; [incanter/incanter-core "1.9.1"]
  ;; [incanter/incanter-charts "1.9.1"]
  ;; [incanter/incanter-io "1.9.1"]
  ;; [clj-stacktrace "0.2.4"]
  ;; [commons-codec "1.5"]
  ;; [colt "1.2.0"]
  ;; [commons-fileupload "1.2.2"]
  ;; ;; [commons-fileupload "1.2.1" :classifier "javadoc"]
  ;; [commons-io "2.0.1"] ;; 1.4

  :library-path "lib")

;; http://clojars.org/repo/
;; http://repo2.maven.org/maven2/
