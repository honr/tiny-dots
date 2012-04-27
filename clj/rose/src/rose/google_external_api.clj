(ns rose.google-external-api
  (:require [clj-http.client :as http.client]
	    [clojure.data.json :as json])
  (:import [java.net URLEncoder]))

;; http://code.google.com/apis/ajaxsearch/documentation/reference.html
;; http://code.google.com/apis/ajaxsearch/documentation/reference.html#_intro_fonje
;; http://code.google.com/apis/ajaxsearch/documentation/

(defn call [search-type m]
  (http.client/get
   (str "http://ajax.googleapis.com/ajax/services/"
	(or
	 ({:w "search/web", :l "search/local", :v "search/video",
	   :b "search/blogs", :n "search/news", :k "search/books",
	   :i "search/images", :p "search/patent",
	   :lt "language/translate", :ld "language/detect"}
	  search-type)
         search-type
	 "web")
	"?v=1.0"
	(apply str
	       (for [[k v] m]
		 (str \& (name k)
		      \= (URLEncoder/encode (str v) "UTF-8")))))))

(defn ajax-call- [search-type m]
  (json/read-json
   (:body (call search-type m))))

(defn ajax-call [search-type m]
  (:responseData (ajax-call- search-type m)))

(defn ajax-search [search-type m]
  (if (m :rsz)
    (let [max-rsz 8]
      (mapcat
       (fn [start]
	 (:results
	  (ajax-call search-type (assoc m
					  :rsz max-rsz
					  :start start))))
       (range 0 (m :rsz) max-rsz)))
    (:results
     (ajax-call search-type  m))))

;; (:unescapedUrl (first (ajax-search :i {:q "Someone"})))

;; Standard URL Arguments

;; :userip   userip=192.168.0.1
;; This argument supplies the IP address of the end-user on whose
;; behalf the request is being made. Requests that include it are less
;; likely to be mistaken for abuse. In choosing to utilize this
;; parameter, please be sure that you're in compliance with any local
;; laws, including any laws relating to disclosure of personal
;; information being sent.

;; :rsz   rsz=4
;; This optional argument supplies the number of results that the
;; application would like to recieve. Values can be any integer
;; between 1 and 8. Alternately, a value of small indicates a small
;; result set size or 4 results. A value of large indicates a large
;; result set or 8 results. Finally, for filter Custom Search Engines,
;; a value of filtered_cse will return 10 results. If this argument is
;; not supplied, a value of small is assumed.

;; :hl    hl=fr
;; This optional argument supplies the host language of the
;; application making the request. If this argument is not present
;; then the system will choose a value based on the value of the
;; Accept-Language http header. If this header is not present, a value
;; of en is assumed.

;; :key   key=your-key
;; This optional argument supplies the application's key. If
;; specified, it must be a valid key associated with your site which
;; is validated against the passed referer header. The advantage of
;; supplying a key is so that we can identify and contact you should
;; something go wrong with your application. Without a key, we will
;; still take the same appropriate measures on our side, but we will
;; not be able to contact you. It is definitely best for you to pass a
;; key.

;; :start    start=4
;; This optional argument supplies the start index of the first search
;; result. Each successful response contains a cursor object (see
;; below) which includes an array of pages. The start property for a
;; page may be used as a valid value for this argument. For reference,
;; a sample cursor object is shown below:
;; "cursor": {
;;   "pages": [
;;     { "start": "0", "label": 1 },
;;     { "start": "4", "label": 2 },
;;     { "start": "8", "label": 3 },
;;     { "start": "12","label": 4 } ],
;;   "estimatedResultCount": "48758",
;;   "currentPageIndex": 0,
;;   "moreResultsUrl": "http://www.google.com/search..."
;; }
      
;; :callback    callback=foo
;; This optional argument alters the standard response format. When
;; supplied, instead of producing a simple JSON encoded object, the
;; system produces a Javascript function call response where the value
;; of callback specifies the name of the function called in the
;; response.
;; callbackFunction(
;;   {"responseData" : {
;;       "results" : [],
;;       "cursor" : {}
;;     },
;;     "responseDetails" : null | string-on-error,
;;     "responseStatus" : 200 | error-code
;; });

;; :context    context=bar
;; This optional argument is related to the context argument. When
;; both are supplied, the value of context alters the normal response
;; format associated with callback. The new format is:
;; callbackFunction(
;;   contextValue,    // the context arg value
;;   responseObject,  // the collection of results and cursor
;;   responseStatus,  // 200 on success, non-200 on failure
;;   errorDetails)    // error string for non-200 response

(defn ajax-search-lucky [search-type m]
  (first (ajax-search search-type (assoc m :rsz 1))))

;; --- images search specific arguments -----------------------
;; safe 	the search safety level
;; active, moderate (default), off
;; ------------------------------------------------------------
;; imgsz 	restrict to images of the specified size
;;     * imgsz=icon - restrict to small images
;;     * imgsz=small|medium|large|xlarge - restrict to medium images
;;     * imgsz=xxlarge - restrict to large images
;;     * imgsz=huge - restrict to extra large images
;; ------------------------------------------------------------
;; imgc 	restrict to images of the specified colorization
;; gray, color
;; ------------------------------------------------------------
;; imgcolor  New! (experimental) 	filter the search to images of the specified color:
;;    black blue brown gray green orange pink purple red teal white yellow
;; ------------------------------------------------------------
;; imgtype (experimental)  restrict to images of the specified type
;; face, photo, clipart, lineart
;; ------------------------------------------------------------
;; as_filetype 	restrict to images of the specified filetype
;; jpg, png, gif, bmo
;; ------------------------------------------------------------
;; as_rights 	restrict to images labeled with the given licenses:
;; cc_publicdomain, cc_attribute, cc_sharealike, cc_noncommercial, cc_nonderived
;; These restrictions can be used together, both positively or negatively. For instance, to emulate the commercial use with modification license, set the following:
;; &as_rights=(cc_publicdomain|cc_attribute|cc_sharealike).-(cc_noncommercial|cc_nonderived)
;; Note: Images returned with this filter may still have conditions on the license for use. Please remember that violating copyright is strictly prohibited by the API Terms of Use. For more details, see this article.
;; ------------------------------------------------------------
;; as_sitesearch 	restrict to images within the specified domain, e.g., as_sitesearch=photobucket.com. Note: This method restricts results to images found on pages at the given URL.
;; ------------------------------------------------------------


(def
  ^{:doc "abbreviated names of languages for the language/translation service"}
  langs-map
  {"auto" "Automatic Language Detection"
   "af" "Afrikaans"
   "ar" "Arabic"
   "az" "Azerbaijani"
   "be" "Belarusian"
   "bg " "Bulgarian"
   "ca" "Catalan"
   "cs" "Czech"
   "cy" "Welsh"
   "da" "Danish"
   "de" "German"
   "el" "Greek"
   "en" "English"
   "es" "Spanish"
   "et" "Estonian"
   "eu" "Basque"
   "fa" "Persian"
   "fi" "Finnish"
   "fr" "French"
   "ga" "Irish"
   "gl" "Galician"
   "hi" "Hindi"
   "hr" "Croatian"
   "ht" "Haitian Creole"
   "hu" "Hungarian"
   "hy" "Armenian"
   "id" "Indonesian"
   "is" "Icelandic"
   "it" "Italian"
   "iw" "Hebrew"
   "ja" "Japanese"
   "ka" "Georgian"
   "ko" "Korean"
   "la" "Latin"
   "lt" "Lithuanian"
   "lv" "Latvian"
   "mk" "Macedonian"
   "ms" "Malay"
   "mt" "Maltese"
   "nl" "Dutch"
   "no" "Norwegian"
   "pl" "Polish"
   "pt" "Portuguese"
   "ro" "Romanian"
   "ru" "Russian"
   "sk" "Slovak"
   "sl" "Slovenian"
   "sq" "Albanian"
   "sr" "Serbian"
   "sv" "Swedish"
   "sw" "Swahili"
   "th" "Thai"
   "tl" "Filipino"
   "tr" "Turkish"
   "uk" "Ukrainian"
   "ur" "Urdu"
   "vi" "Vietnamese"
   "yi" "Yiddish"
   "zh-CN" "Chinese"})
