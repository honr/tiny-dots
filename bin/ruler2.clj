#!/usr/bin/env clove
;; | clojure

;;; Description:
;;;
;;;   Looks for the file `rule.clj' in the current directory or above until it
;;;   reaches a file-system boundary (if in $HOME go up upto $HOME, otherwise
;;;   go up upto /).  If there is no such `rule.clj' file, do nothing. (TODO:
;;;   actually exit when not found).
;;;   Expects a map named `rules' in said `rule.clj', which looks like this:
;;;     {"foo.bar" '(DERIVING-FN dep_1 dep_2 ... dep_n)
;;;      "baz" '(OTHER-DERIVING-FN dep_1 dep_2)
;;;      ;; And so on.
;;;      }

;;;   We should carefully control the loading, caching and reloading of
;;;   libraries.  We might need to sandbox loadings and runs, together with
;;;   garbage collected classes.

(ns bin.ruler2
  (:refer-clojure :exclude [derive])
  (:require [rose.clu :as clu]
            [rose.file]
            [clojure.java.io]
            [clojure.walk :as walk]
            [clojure.string]
            [clojure.pprint])
  (:use [rose.git-util])
  (:import [java.util.zip DeflaterOutputStream InflaterInputStream]
           [java.io File FileOutputStream FileInputStream ByteArrayOutputStream]
           [java.security MessageDigest]))

(def store-subdir "/.r-store")

;; Internal.
(defonce global-cache-map (ref {}))

;; Internal dynamic.

(def ^{:dynamic true} *rules* {})

;; (def ^{:dynamic true} *rules-tree*)

(def ^{:dynamic true} *cache*)  ;; A ref.

;;; Utilities

(defn filter-prefix [prefix coll]
  (filter #(.startsWith % prefix) coll))

(defn create-necessary-dirs []
  "Assumes *dir* is set properly."
  (make-sure-directory-exists (str *dir* store-subdir "/objects"))
  (make-sure-directory-exists (str *dir* store-subdir "/build"))
  (make-sure-directory-exists (str *dir* store-subdir "/used-chamber")))

;;; Get Rules.

(defn find-rules-file [^String filename ^String path]
  (let [boundary (if (or (= path *home*)
                         (.startsWith path (str *home* \/)))
                   *home*
                   "/")]
    (first
     (filter file-exists?
             (map (fn [x]
                    (str x "/" filename))
                  (take-while
                   (fn [x]
                     (and x (.startsWith x boundary)))
                   (iterate file-parent path)))))))

(defn find-rules-dir []
  (.getParent (File. (find-rules-dir "rule.clj" *cwd*))))

(defn split-path [^String p]
  (loop [p p coll []]
    (let [n (inc (.indexOf p "/"))]
      (if (zero? n)
        (conj coll p)
        (recur (.substring p n) (conj coll (.substring p 0 n)))))))

(defn make-path-tree-fn [m ^String x]
  "Useful for creating a path tree from a collection of paths,
 e.g.: (reduce make-path-tree-fn {} coll)."
  (assoc-in m (split-path x) x))

(defn read-rules []
  "Read rules from the file rule.clj and return the map
 {:ns NS, :dir DIRECTORY, :publics NS-PUBLICS,
  :rules RULES,
  :targets-tree TARGETS-TREE, :specified-targets-tree SPECIFIED-TARGETS-TREE},
 where NS is the namespace defined in the rule.clj, NS-PUBLICS is a collection
 of its public symbols, DIRECTORY is where the file rule.clj is, RULES is a
 map from targets to each one's derivation rule, targets-tree is a tree of all
 targets, and specified-targets-tree is a similar tree with only specified
 target.  Please make sure the path to your rule.clj file is indirectly (e.g.,
 symlinked path) or directly in the classpath.  Normally you should bind
 *rules* to the :rules one in the return value of read-rules."
  (when-let [rule-file (find-rules-file "rule.clj" *cwd*)]
    (let [rule-ns-stub (read
                        (clojure.lang.LineNumberingPushbackReader.
                         (clojure.java.io/reader rule-file)))
          rule-ns (symbol (second rule-ns-stub))]
      (require [rule-ns :reload true])
      (let [ns-specified-rules (var-get (ns-resolve rule-ns 'rules))
            ns-nodes (set (mapcat (fn [[k rule]]
                                    (conj (filter string? rule) k))
                                  ns-specified-rules))
            ns-rules (merge
                      (into {}
                            (for [node ns-nodes]
                              [node ()]))
                      ns-specified-rules)]
        {:ns rule-ns
         :dir (.getParent (File. rule-file))
         :publics (ns-publics rule-ns)
         :rules ns-rules
         :targets-tree (reduce make-path-tree-fn {} (keys ns-rules))
         :specified-targets-tree (reduce make-path-tree-fn {} (keys ns-specified-rules))}))))

(defn completion-suggestions [p tree]
  (let [cur-vec (split-path p)
        cur-vec-prefix (butlast cur-vec)
        cur-vec-prefix-str (apply str cur-vec-prefix)
        suggestions (map (fn [x]
                           (str cur-vec-prefix-str x))
                         (filter-prefix (last cur-vec)
                                        (keys (get-in tree cur-vec-prefix))))]
    (if (and (not-empty suggestions)
             (not (next suggestions))
             (.endsWith (first suggestions) "/"))
      ;; There is single suggestion.
      (let [suggestion (first suggestions)]
        [(.substring suggestion 0 (dec (count suggestion))) suggestion])
      ;; There are multiple suggestions or no suggestions at all.
      suggestions)))

(deftype RulerBuildTarget [nom]
  rose.complete/compable
  (complete [this]
    (completion-suggestions (.nom this) (get (read-rules) :specified-targets-tree))))

;;; Git related.



;;; Rest of it.

;; Note: Currently all nodes represent files.  There is no virtual target.



;;; Working with changes.

(defn node-fresh? [node-rules node]
  ;; Does not modify *cache*.
  (let [blob-index (get @*cache* :blob-index)
        node-blobs (get blob-index node)]
    (and (not-empty node-blobs)
         (= (first node-blobs)
            (get-in @*cache* [:memo node (vec
                                          (map (fn [x]
                                                 (if (string? x)
                                                   (first (get blob-index x))
                                                   x))
                                               node-rules))])))))

(defn nodes-build-level [rules nodes]
  (loop [levels-map {} node-stack nodes]
    (if (empty? node-stack)
      levels-map
      (let [node (first node-stack)]
        (cond (levels-map node)  ;; Already assigned a level.  Move on.
              (recur levels-map (rest node-stack))

              (empty? (rules node)) ;; Does not have any rebuilding option.  Leave as is!
              (recur (assoc levels-map node 0) (rest node-stack))

              :else
              (let [deps-levels-of-node (map levels-map
                                             (filter string? (rules node)))]
                (cond (empty? deps-levels-of-node)  ;; No file deps.
                      (recur (assoc levels-map
                               node (if (node-fresh? (rules node) node) 0 1))
                             (rest node-stack))

                      ;; Seen all deps.  If all sub-tree levels are zero, we
                      ;; have to look at freshness of the node.  Otherwise,
                      ;; the level of this node is one plus the maximum level
                      ;; of sub nodes.
                      (every? identity deps-levels-of-node)
                      (recur (assoc levels-map
                               node (let [l-max (reduce max deps-levels-of-node)]
                                      (if (pos? l-max)
                                        (inc l-max)
                                        (if (node-fresh? (rules node) node) 0 1))))
                             (rest node-stack))

                      :else  ;; There are deps that need to be seen.
                      (recur levels-map (concat
                                         (filter (fn [x]
                                                   (and (string? x)
                                                        (not (levels-map x))))
                                                 (rules node))
                                         node-stack)))))))))

(defn nodes-build-order [nodes]
  (let [m (nodes-build-level *rules* nodes)
        m-inv (group-by m (keys m))]
    (map m-inv (sort (filter pos? (keys m-inv))))))

;;; Derivation procedure.

(defn build-chamber-set-up [room-number blob-index file-deps]
  (let [build-dir (format "%s%s/build/%03d" *dir* store-subdir room-number)]
    (doseq [dep file-deps]
      (make-sure-parent-directory-exists (str build-dir \/ dep))
      (extract-blob-to (str *dir* store-subdir)
                       (first (get blob-index dep))
                       (str build-dir \/ dep)))
    build-dir))

(defn build-chamber-clean-up [room-number node-sha1]
  (make-sure-parent-directory-exists
   (str *dir* store-subdir
        "/used-chamber/" (insert-slash-at-2 node-sha1)))
  (.renameTo (File. (format "%s%s/build/%03d" *dir* store-subdir room-number))
             (File. (str *dir* store-subdir
                         "/used-chamber/" (insert-slash-at-2 node-sha1)))))

(defn derive-node [rules-ns node-rules node]
  ;; Modifies *cache*.
  (let [blob-index (get @*cache* :blob-index)
        room-number 1
        build-dir (build-chamber-set-up room-number
                                        (get @*cache* :blob-index)
                                        (filter string? node-rules))]
    (binding [*cwd* build-dir]
      (make-sure-parent-directory-exists (str *cwd* \/ node))
      (let [resolved-node-rules (map (fn [x]
                                       (if (symbol? x)
                                         (var-get (ns-resolve rules-ns x))
                                         x))
                                     node-rules)]
        (apply (first resolved-node-rules)
               node
               (next resolved-node-rules)))
      (let [node-sha1 (rose.git-util/str-hex
                       (write-blob (str *dir* store-subdir)
                                   (slurp-bytes *cwd* node)))]
        (dosync
         (alter *cache*
                update-in [:blob-index node]
                conj node-sha1)
         (alter *cache*
                update-in [:memo node]
                assoc (vec (map (fn [x]
                                  (if (string? x)
                                    (first (get blob-index x))
                                    x))
                                node-rules))
                node-sha1))
        (build-chamber-clean-up room-number node-sha1)))))

(defn- load-cache-internal [rules-ns rules-dir]
  (binding [rose.git-util/*dir* rules-dir]
    (let [cache-file (str *dir* store-subdir "/cache.clj")]
      (dosync
       (alter global-cache-map
              assoc
              rules-ns
              (ref (if (file-exists? cache-file)
                     (read-string
                      (slurp
                       (str *dir* store-subdir "/cache.clj")))
                     {:blob-index {} :memo {} :last-known-treeish nil})))))))

(defn- save-cache-internal [rules-ns rules-dir]
  (binding [rose.git-util/*dir* rules-dir]
    (create-necessary-dirs)
    (spit (str *dir* store-subdir "/cache.clj")
          @(get @global-cache-map rules-ns))))

;;; Commands

(defn load-cache {:cli {}} []
  (let [ns-rules (read-rules)]
    (load-cache-internal (ns-rules :ns) (ns-rules :dir))))

(defn save-cache {:cli {}} []
  (let [ns-rules (read-rules)]
    (save-cache-internal (ns-rules :ns) (ns-rules :dir))))

(defn start-at-treeish {:cli {}} [treeish]
  (let [ns-rules (read-rules)]
    (dosync
     (alter global-cache-map
            assoc
            (ns-rules :ns)
            (ref {:blob-index {} :memo {} :last-known-treeish treeish})))))

(defn notice-treeish {:cli {}} [treeish]
  (let [ns-rules (read-rules)]
    (load-cache-internal (ns-rules :ns) (ns-rules :dir))
    (binding [*rules* (ns-rules :rules)
              rose.git-util/*dir* (ns-rules :dir)
              ;; *rules-tree* (ns-rules :targets-tree)
              *cache* (get @global-cache-map (ns-rules :ns))]
      (update-cache-index-git store-subdir
                              (set (keys *rules*))
                              (git-treeish-sha1 *dir* treeish)
                              *cache*))
    (save-cache-internal (ns-rules :ns) (ns-rules :dir))))

(defn registred-objects {:cli {:load Boolean :sha1 Boolean}} []
  (let [ns-rules (read-rules)]
    (when [rose.clu/*opts*]
      (load-cache-internal (ns-rules :ns) (ns-rules :dir)))
    (binding [*rules* (ns-rules :rules)
              rose.git-util/*dir* (ns-rules :dir)
              *cache* (get @global-cache-map (ns-rules :ns))]
      (if (rose.clu/*opts* :sha1)
        (doseq [[k v] (get @*cache* :blob-index)]
          (println k " : " (first v)))
        (println
         (keys (get @*cache* :blob-index)))))))

(defn derive {:cli {}} [^RulerBuildTarget & targets]
  "Using git state and saved caches rebuild the targets (just once; Not
monitoring and actively keeping them up-to-date)."
  (let [ns-rules (read-rules)]
    (load-cache-internal (ns-rules :ns) (ns-rules :dir))
    (binding [*rules* (ns-rules :rules)
              rose.git-util/*dir* (ns-rules :dir)
              ;; *rules-tree* (ns-rules :targets-tree)
              *cache* (get @global-cache-map (ns-rules :ns))]
      (update-cache-index-git store-subdir
                              (set (keys *rules*))
                              nil
                              *cache*)
      (create-necessary-dirs)
      (doseq [batch (nodes-build-order targets)]
        (doseq [node batch]
          (println "# Deriving node" node)
          (derive-node (ns-rules :ns) (get *rules* node) node)))
      (doseq [node targets]
        (if (not (get *rules* node))
          (println "Unknown target:" node)
          (if (empty? (get *rules* node))
            (println "No derivation rule for target:" node)
            (do
              (extract-blob-to (str *dir* store-subdir)
                               (first (get (get @*cache* :blob-index) node))
                               (str *dir* \/ node))
              (println "Activating" node))))))
    (save-cache-internal (ns-rules :ns) (ns-rules :dir))))

(defn hide-derivables {:cli {}} []
  (let [ns-rules (read-rules)]
    (binding [*rules* (ns-rules :rules)
              rose.git-util/*dir* (ns-rules :dir)]
      (doseq [node (keys *rules*)]
        (when (not-empty (get *rules* node))
          (rose.file/rm (str *dir* \/ node)))))))

;; (defn update-index-git {:cli {}} []
;;   "Voluntary report of updated files, using git."
;;   )

;; (defn update-cache-index-inotfy [*rules* interesting-targets]
;;   ;; TODO...
;;   )

;; (defn maintain-targets {:cli {}} [& targets]
;;   "Monitor dependencies and maintain targets up-to-date.  Uses inotify."
;;   )

;;; End.
(rose.clu/run-command-maybe-ns *ns* "ruler2.clj")
