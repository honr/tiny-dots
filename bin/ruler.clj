#!/usr/bin/env clove
;; | clojure

;;; Description:
;;;
;;;   Looks for the file rule.clj in the current directory or above
;;;   until it reaches a file-system boundary (if in $HOME go up upto
;;;   $HOME, otherwise go up upto /).  If there is no such rule.clj do
;;;   nothing.
;;;   Expects a map named rules in said rule.clj, which looks like this:
;;;     {"foo.bar" '(DERIVING-FN dep_1 dep_2 ... dep_n)
;;;      "baz" '(OTHER-DERIVING-FN dep_1 dep_2)
;;;      ;; And so on.
;;;      }
;;;   For each target we ...
;;;

(ns bin.ruler
  (:refer-clojure :exclude [test derive])
  (:require [rose.clu]
            [rose.file]
            [clojure.java.io :as jio])
  (:import [java.util.zip DeflaterOutputStream]
           [java.io File FileOutputStream ByteArrayOutputStream]
           [java.security MessageDigest]))



(defn find-rules-file [^String filename ^String path ^String home]
  (let [boundary (if (and (not= path home)
                          (.startsWith path home))
                   home
                   "/")]
    (first
     (filter
      (fn [x] (.exists (File. x)))
      (map (fn [x] (str x "/" filename))
       (take-while
        (fn [x] (and x (.startsWith x boundary)))
        (iterate
         (fn [x] (.getParent (File. x)))
         path)))))))

(defn read-rules []
  "Read rules from the file rule.clj and return the map
 {:ns NS, :dir DIRECTORY, :publics NS-PUBLICS},
 where NS is the namespace defined in the rule.clj,  NS-PUBLICS is a collection
 of its public symbols, and DIRECTORY is where the file rule.clj is.  Please
 make sure the path to your rule.clj file is indirectly or directly in the
 classpath."
  (when-let [rule-file (find-rules-file "rule.clj" *cwd* *home*)]
    (let [rule-ns-stub (read
                        (clojure.lang.LineNumberingPushbackReader.
                         (jio/reader rule-file)))
          rule-ns (symbol (second rule-ns-stub))]
      (require [rule-ns :reload true])
      {:ns rule-ns
       :dir (.getParent (File. rule-file))
       :publics (ns-publics rule-ns)})))

(defn filter-prefix [prefix coll]
  (filter #(.startsWith % prefix) coll))

(defn make-sure-parent-directory-exists [p]
  (let [d (.getParentFile (File. p))]
    (when-not (.exists d)
      (.mkdirs d))))

(defn str-hex [bytes]
  (apply str (map #(format "%02x" %) bytes)))

(defn write-object [git-path content-type content]
  "content-type is :blob, :tree, :commit, ?
   content is a byte array which can be obtained by
   (.getBytes some-string \"UTF-8\") if input is a string."
  (let [store (.toByteArray
               (doto (ByteArrayOutputStream.)
                 (.write (.getBytes (format "%s %d\0"
                                            (name content-type)
                                            (count content))
                                    "UTF-8"))
                 (.write content)))
        digest (.digest
                (MessageDigest/getInstance "SHA-1")
                store)
        blob-path (str git-path "/objects/"
                       (str-hex [(first digest)])
                       \/
                       (str-hex (next digest)))]
    (make-sure-parent-directory-exists blob-path)
    (with-open [output (DeflaterOutputStream.
                         (FileOutputStream.
                          blob-path))]
      (.write output store))
    digest))

(defn write-blob [git-path content]
  (write-object git-path :blob (.getBytes content "UTF-8")))

(defn write-tree [git-path coll]
  "Example:
 (write-tree git-path
   [{:filename \"hello\" :mode \"100644\" :content \"Hello\n\"}])"
  (let [content-ba (ByteArrayOutputStream.)]
    (doseq [x coll]
      (let [digest (write-object git-path
                                 :blob
                                 (.getBytes (:content x) "UTF-8"))]
        (.write content-ba (.getBytes
                            (format "%s %s\0"
                                    (:mode x)
                                    (:filename x))
                            "UTF-8"))
        (.write content-ba digest)))
    (write-object git-path :tree (.toByteArray content-ba))))

;; ;; Insert stuff into *rules*
;; (defmacro defrule [target args] body)
;; (defmacro defrule-fn [target] lambda)

(def ^{:dynamic true} *rules* nil)

(def ^{:dynamic true} *derivations-cache* nil)

(def ^{:dynamic true} *cache-obsolete*)

(def ^{:dynamic true} *rules-directory* nil)

(defn drv-helper [target]
  ;; Side effects: updates *cache-obsolete* and *derivations-cache*.

  (when (nil? (@*cache-obsolete* target))
    (let [[f & deps] (*rules* target)]
      (if (empty? deps)                 ; Leaf.
        (do
          (dosync
           (alter *cache-obsolete*
                  assoc-in [target]
                  ;; File unlike cache:
                  (let [file (File. *rules-directory* target)]
                    (cond (not (get @*derivations-cache* target))
                          :new

                          (not (.exists file))
                          :not-exists

                          (not= (get @*derivations-cache* target)
                                (hash (slurp file)))
                          :hash-mismatch

                          :else
                          false))))
          nil)                          ; Return nil.
        (let [to-rederive (doall (mapcat drv-helper deps))]
          (when (or (some @*cache-obsolete* deps)
                    (not (.exists (File. *rules-directory* target))))
            (dosync
             (alter *cache-obsolete*
                    assoc-in [target] :cache-miss-deps))
            (cons target to-rederive)))))))

(defn drv-build-and-register [derivation-fn target deps]
  (apply derivation-fn target deps)
  (let [target-file (File. *rules-directory* target)]
    (when (.exists target-file)
      (doseq [filename (cons target (filter @*cache-obsolete* deps))]
        (let [file-hash (hash (slurp (File. *rules-directory* filename)))]
         (dosync
          (alter *derivations-cache* assoc-in [filename] file-hash)
          (alter *cache-obsolete* dissoc filename)))))))

(defn drv [target]
  (binding [*cache-obsolete* (ref {})]
    (doseq [intermediate-target (reverse
                                 (drv-helper target))]
      (let [[derivation-fn & deps] (get *rules* intermediate-target)]
        (println "derive:" intermediate-target "- deps:" deps)
        (drv-build-and-register derivation-fn intermediate-target deps)))
    @*cache-obsolete*))

;; TODO: Needs its own rule completion:
;;   look for a file named RULE.clj in current dir, or limited-parent
;;   (Note: limited-parent: if we are under *home*, go up until we
;;   reach *home*.  otherwise go up until / ).  Once we get the
;;   RULE.clj, load it as a library.  (Note: We need to carefully
;;   control the loading, caching and reloading of libraries.  We
;;   might need to sandbox loadings and runs, together with garbage
;;   collected classes). Look for targets in the loaded ns.  [Optional
;;   Design Decision] Each target is a cli command itself.  Targets
;;   are expected to be hand-sorted to not depend on non-yet-defined
;;   targets.

(deftype RulerBuildTarget [nom]
  rose.complete/compable
  (complete [this]
    (let [cur (.nom this)]
      (filter-prefix cur (map #(str (first %))
                              (:publics (read-rules)))))))

(defn build {:cli {}} [^RulerBuildTarget targ & args]
  (let [rules-ns (read-rules)]
    (binding [*cwd* (rules-ns :dir)]    ; Some libraries are still ignoring
                                        ; *cwd*.  Must fix them!
      (apply (ns-resolve (rules-ns :ns) (symbol targ)) args))))

(defn test {:cli {}} [& args]
  (println "... going to test ..."))

(defonce large-derivations-cache (ref {}))

(defn derive {:cli {}} [target]
  (let [their-rules (read-rules)
        their-ns (:ns their-rules)]
    (dosync
     (alter large-derivations-cache
            update-in [their-ns]
            #(if % % (ref {}))))
    (binding [*rules* (var-get (ns-resolve their-ns 'rules))
              *rules-directory* (:dir their-rules)
              *derivations-cache* (get @large-derivations-cache their-ns)
              *cache-obsolete* (ref {})]
      (drv target))))

(defn reset-cache {:cli {}} []
  (let [their-rules (read-rules)
        their-ns (:ns their-rules)]
    (println "Cache was:")
    (println " " @(get @large-derivations-cache their-ns))
    (dosync
     (alter large-derivations-cache
            assoc-in [their-ns] (ref {})))))

(defn ^{:cli {}} clean-intermediate [target]
  "Remove intermediate files.  Does not garbage-collect stored objects."
  (rose.clu/sh "rm" "foo.o" "foo"))

(rose.clu/run-command-maybe-ns *ns* "ruler.clj")
