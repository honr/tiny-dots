#!/usr/bin/env clove
;; | clojure

(ns bin.weather
  (:require [rose.clu :as clu]
	    [rose.xml :as xml]
	    [clojure.string :as string]
	    [clojure.set]
            [clojure.java.jdbc :as sql]
	    [clojure.pprint]
	    [clj-http.client :as http.client])
  (:use [rose.utils :only [replace-batch]]))

(defmacro db [& body]
  `(sql/with-connection
     ~{:classname "org.sqlite.JDBC"
       :subprotocol "sqlite"
       :subname (rose.file/path :home ".local/var/run/weather.sqlite")}
     ~@body))

(defn db-query [q]
  "query the database. `q' must be a vector."
  (db (sql/with-query-results res q (into [] res))))

(comment
  (db (sql/create-table :canada
			[:key :integer :primary :key]
			[:loc :text]
			[:suburl :text]))
  (db (sql/drop-table :canada)))

(defn- update-table-canada-suburl []
  (db
    (sql/do-commands "DELETE FROM canada")
    (apply sql/insert-values :canada
	   [:loc :suburl]
	   (filter identity
		   (map
		    #(when-let [[k v] (when % (.split % "\\|" 2))]
		       [k (.toLowerCase v)])
		    (mapcat
		     (fn [province-line]
		       (when-let [row (second
				       (re-matches #"citiesEn\['[A-Z]*'\]=(.*);"
						   province-line))]
			 (read-string row)))
		     (string/split
		      (slurp (str "http://www.weatheroffice.gc.ca"
				  "/city/js/forecastQuickLinkArraysEn.js")
			     :encoding "ISO-8859-1")
		      #"\n")))))))

(defn- get-weather-canada [loc loc-m]
  (let [s (:suburl loc-m)
        src-url (str "http://www.weatheroffice.gc.ca/city/pages/" s)
	current-cond (xml/sel
		      [(xml/m<-s
			(rose.utils/replace-batch
			 (:body
			  (http.client/get src-url))
			 #"&ccedil;" "ç"
			 #"&nbsp;" " "
			 #"&deg;" "°"
			 ))]
		      :html :body
		      "div.page div.core div.colLayout div.center"
		      "div#container div#mainContent div#conditionscontainer"
		      "div#currentcond")]
    {:url src-url
     :weather
     (clojure.set/rename-keys
      (merge
       (let [x (xml/sel current-cond
			"div#currentcond-content div#cityobserved"
			:dl)]
	 {:observation-station (first (xml/sel x 1))
	  :observation-date (first (xml/sel x 3))})

       (let [x (filter #(not= (:class (:attrs %)) "dd1")
		       ;; ^ work around quirk!
		       (xml/sel current-cond
				"div#currentcond-content div#citycondition"
				:ul "li.leftList dl.leftCol"))
	     y (filter #(not= (:class (:attrs %)) "dd1")
		       ;; ^ work around quirk!
		       (xml/sel current-cond
				"div#currentcond-content div#citycondition"
				:ul "li.rightList dl.rightCol"))
	     xy (concat (partition 2 x) (partition 2 y))]
	 (into {} (for [[k v] xy]
		    (let [k (xml/contentize k)]
		      [(clojure.string/trim
			(clojure.string/replace (if (coll? k) (last k) k)
						":" ""))
		       (let [v (xml/contentize v)]
			 (cond (empty? v) "?"
			       (string? v) v
			       (coll? v) (into [] v)))])))))
      {"Air Quality Health Index" :air-quality
       "Temperature" :temperature
       "Wind" :wind
       "Wind Chill" :windchill
       "Dewpoint" :dewpoint
       "Humidity" :humidity
       "Pressure" :pressure
       "Tendency" :temperature-tendency
       "Condition" :condition
       "Visibility" :visibility})}))

(def location-m
  (merge
   {"ca/Toronto" #(get-weather-canada "on-143_metric_e.html")} ;; default
   (into {} ;; Canada
	 (for [{k :loc suburl :suburl} (db-query ["select * from canada"])]
	   [(str "ca/" k) {:fn get-weather-canada
                           :suburl (str suburl "_metric_e.html")}]))))

;; (keys location-m)
;; (location-m "Québec")
;; (location-m "Gjoa Haven")

;; TODO: create db
(deftype bin.weather.locationT [nom]
  rose.complete/compable
  (complete [this]
    (filter #(.startsWith % (.nom this))
            (keys location-m))))

(defn main {:cli-default true
	    :cli {}
            ;; {:location [(set (keys location-m))] :l :location}
            }
  [^locationT loc]
  (let [;; loc (clu/*opts* :location)
        loc-m (get location-m loc)
        w ((:fn loc-m) loc loc-m)]
    (println "from" (clu/ascii-color 0 :d (:url w)))
    (doseq [[k v] (sort-by #(str (first %)) (:weather w))]
      (println (clu/ascii-color :y (format "%24s" (cond (string? k) (with-out-str (pr k))
                                        ; to help us fix it
						    (keyword? k) (name k)
						    :else (str k))))
	       (clu/ascii-color :y :b v)))))

(clu/run-command-maybe)
