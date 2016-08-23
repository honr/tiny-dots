(defproject rose "0.0.1-SNAPSHOT"
  :description "a library of useful small tools"
  :dependencies [[org.clojure/clojure "1.8.0"]
		 ;; [org.apache.httpcomponents/httpclient "4.0.1"] ;; for [clj-http]:

		 [commons-codec "1.5"]
		 [commons-fileupload "1.2.2"]
		 [commons-io "2.0.1"]
		 [javax.mail/mail "1.4.4"]
		 [org.mortbay.jetty/jetty "6.1.15"]
     [postgresql "9.1-901.jdbc4"] ;; "8.4-702.jdbc4"
		 [xpp3 "1.1.4c"]
		 [compojure "1.4.0"]
		 ;; [hiccup "0.4.0-SNAPSHOT"]
     [ring/ring-jetty-adapter "1.4.0"]
     [ring/ring-servlet "1.4.0"]
		 [clout "1.0.1"]

		 [clj-http "2.0.0"]
		 [joda-time "2.9.1"]
		 ;;[tagsoup "1.2"]
		 ])
