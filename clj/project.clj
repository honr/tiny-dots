(defproject jar-store "0.0.0"
  :description "virtual package for obtaining all dependencies"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/algo.generic "0.1.2"]
                 [org.clojure/algo.monads "0.1.6"]
                 [org.clojure/core.async "0.3.465"]
                 [org.clojure/core.cache "0.6.5"]
                 [org.clojure/core.logic "0.8.11"]
                 ;; [org.clojure/core.match "0.2.2"]
                 [org.clojure/core.memoize "0.5.9"]
                 [org.clojure/core.typed "0.4.3"]
                 ;; [org.clojure/core.unify "0.5.6"]
                 [org.clojure/data.csv "0.1.4"]
                 ;; [org.clojure/data.codec "0.1.1"]
                 [org.clojure/data.json "0.2.6"]
                 ;; [org.clojure/data.xml "0.2.0-alpha5"]
                 [org.clojure/data.zip "0.1.2"]
                 ;; [org.clojure/java.classpath "0.2.3"]
                 ;; [org.clojure/java.classpath "0.2.3"]
                 [org.clojure/java.jdbc "0.7.4"]
                 [org.clojure/test.check "0.9.0"]
                 [org.clojure/tools.analyzer "0.6.9"]
                 [org.clojure/tools.analyzer.jvm "0.7.1"]
                 [org.clojure/tools.cli "0.3.5"]
                 ;; [org.clojure/tools.emitter.jvm ""]
                 [org.clojure/tools.logging "0.4.0"]
                 ;; [org.clojure/tools.macro "0.1.5"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [org.clojure/tools.nrepl "0.2.13"]
                 [org.clojure/tools.reader "1.1.1"]
                 ;; [org.clojure/tools.trace "0.7.9"]
                 ;; [swank-clojure "1.4.2"]
                 [org.mozilla/rhino "1.7.7.2"]

                 ;; v20160315r and then v20160713 has worked before this.
                 [com.google.javascript/closure-compiler "v20171203"]
                 [org.clojure/clojurescript "1.9.946"]
                 [com.google.guava/guava "23.5-jre"]
                 [com.google.protobuf/protobuf-java "3.5.0"]
                 [com.google.protobuf/protobuf-lite "3.0.1"]
                 [com.google.protobuf/protoc "3.5.0"]
                 [javax.mail/mail "1.4.7"]
                 [javax.websocket/javax.websocket-api "1.1"]
                 ;; "9.4.0.RC1" might depend on unix sockets.
                 [org.eclipse.jetty/jetty-server "9.4.8.v20171121"]
                 [org.postgresql/postgresql "42.1.4"]
                 [org.ogce/xpp3 "1.1.6"]
                 [org.jsoup/jsoup "1.11.2"]
                 ;; [compojure "1.6.0"]
                 ;; [hiccup "1.0.5"]
                 [ring/ring-jetty-adapter "1.6.3"]
                 [ring/ring-servlet "1.6.3"]
                 ;; [clout "2.1.2"] ; an http route matching library.
                 [org.yaml/snakeyaml "1.19"]

                 [org.xerial/sqlite-jdbc "3.20.0"]
                 [com.cognitect/transit-clj "0.8.300"]
                 ;; [com.cognitect/transit-js "0.8.846"]

                 ;; a bit experimental below here:
                 [io.grpc/grpc-all "1.8.0"]
                 [io.netty/netty-all "4.1.19.Final"]

                 [clj-http "3.7.0"]
                 [joda-time "2.9.9"]
                 [leiningen/leiningen "2.8.1"]
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
