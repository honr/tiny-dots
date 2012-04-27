(ns rose.utils
  (:require [clojure.set]))

(defn ns-syms [x] (keys (ns-interns (the-ns x))))

;; (def *user-dir* (System/getProperty "user.dir"))
;; (def *home-dir* (System/getProperty "user.home"))
(def os-linux? (= (System/getProperty "os.name") "Linux" ))
(def os-macosx? (= (System/getProperty "os.name") "Mac OS X"))

(defmacro sysprop
  ([x] (System/getProperty (str x)))
  ([x y] (System/setProperty (str x) y)))

(defn filter= [f v coll] (filter #(= (f %) v) coll))
(defn ffilter= [f v coll] (first (filter #(= (f %) v) coll)))
(defn filterk= [k v coll] (filter #(= (get % k) v) coll))
(defn ffilterk= [k v coll] (first (filter #(= (get % k) v) coll)))

;;; utils
(defn list-to-ranges [l]
  "monotone list increment at least 1"
  (if (not-empty l)
    (loop [l (sort l) a (first l) b nil r []]
      (if l
	(if (and b (< b (first l))) 
	  (recur (next l) (first l) (inc (first l)) (conj r [a b]))
	  (recur (next l) a (inc (first l)) r))
	(conj r [a b])))))

(defn mapf [& fs] (fn [x] (map #(% x) fs)))
;; ((mapf + -) 10)

(defn transpose [coll]
  (apply map vector coll))
;; not lazy, though.

;; (transpose [[1 2 3] [4 5 6] [7 8 9]]) -> ([1 4 7] [2 5 8] [3 6 9])

;; (defn de-interleave [n coll] (map #(take-nth n %) (take n (iterate next coll))))
(defn de-interleave [n coll] (transpose (partition n coll)))

;; (de-interleave 2 [1 2 3 4 5 6]) ((1 3 5) (2 4 6))
;; (apply interleave(de-interleave 2 [1 2 3 4 5 6]))

(defn mean 
  ([coll]
     (mean identity coll))
  ([f coll]
     (/ (reduce + (map f coll))
	(count coll))))

(defn logspace [a b n]
  (let [la (Math/log a)
	lb (Math/log b)]
    (map #(Math/exp (+ la (* % (- lb la) (/ (- n 1))))) (range n))))

;; (defn bernoulli-mul-expt [p m k]
;;   (* (expt p k) (expt (- 1 p) (- m k))))

(defn select-rename-keys [kmap map]
  (clojure.set/rename-keys (select-keys map (keys kmap)) kmap))

;; (def pool (java.util.concurrent.Executors/newCachedThreadPool))
;; (defn go [& args] (.submit pool #(apply -main args)))

(defn partition-with 
  "Applies f to coll, splitting it each time, and continues with the
   second part as the new coll. Returns a lazy seq of lazy seqs."  
  [f coll]
  (when-let [s (seq coll)]
    (let [[x y] (f s)]
      (lazy-seq
	(cons x (partition-with f y))))))

(defn memoize-cache
  "Returns a memoized version of a referentially transparent
  function. The memoized version of the function keeps a cache of the
  mapping from arguments to results and, when calls with the same
  arguments are repeated often, has higher performance at the expense
  of higher memory use. You can pass your own cache as the second
  argument."

  ([f mem]
     (fn [& args]
       (if-let [e (find @mem args)]
	 (val e)
	 (let [ret (apply f args)]
	   (swap! mem assoc args ret)
	   ret))))
  ([f]
     (let [mem (atom {})]
       (memoize-cache f mem))))

(defn map-map [f m] 
  "Tranform the map m by applying f to every value."
  (into {} (for [[k v] m] [k (f v)])))

;; another implementaion by Chouser in #clojure
;; (defn map-map [f m] (zipmap (keys m) (map f (vals m))))

(defn map-map* [f m]
  "Tranform the map m by applying f to every value.
f takes exactly two arguments, the key and the value,
and returns the new value."
  (let [ff (fn [[k v]] [k (f k v)])]
    (into {} (map ff m))))

(defn fix 
  ([x] x)
  ([pred f x]
     (if (pred x) (f x) x))
  ([pred1 f1 pred2 f2 & args]
     (fix pred1 f1 (apply fix pred2 f2 args))))

(defn assure [preds x]
  (when (every? #(% x) preds) x))

(defn repair-passing [pred-fn-coll x]
  (loop [coll (partition 2 pred-fn-coll), 
	 y x]
    (if (empty? coll)
      y
      (recur (rest coll) 
	     (if ((first (first coll)) y)
	       ((second (first coll)) y)
	       y)))))

(defn repair-full [pred-fn-coll x]
  (let [coll-orig (partition 2 pred-fn-coll)]
   (loop [coll coll-orig, 
	  y x]
     (if (empty? coll)
       y
       (if ((first (first coll)) y)
	 (recur coll-orig ((second (first coll)) y))
	 (recur (rest coll) y))))))

(defn index-by-key [k m]
  "Create a map from a collection of small maps, indexed by the
  selected key."
  (let [mm (ref {})]
    (dosync
     (doseq [x m]
       (alter mm assoc (get x k) x)))
    @mm))

(defn re-parser [re ks]
"Generate a regex-based parser.\nA map is returned with groups stored in corresponding keys (given in\nks). The first key will hold the whole string. When a key is nil (the\nkey itself, not its value), that key and its corresponding value will\nnot appear in the resulting map."
  (fn [s]
    (when s
      (dissoc (zipmap ks (re-matches re s)) nil))))
;; NOTE: in re-matches the whole string must match. To match only part
;;       of the string use re-find.

(defn re-parser-multi [& coll]
  "generate a multi regex-based parser.

  example
 (map (re-parser-multi
        [1 :value \"(?iu)\" [:name \"(.*)\"] \":\" [:name-residue \"(.*)\"] \":\" [:value \"(.*)\"]]
        [2 :value \"(?iu)\" [:name \"(.*)\"] \":\" [:value \"(.*)\"]])
      [\"a:b\" \"c:d:e\" \"f\"])

  we add two keys: :PARSER-ID and :ORIGINAL 
  non-parsed items will have the :PARSER-ID on nil,
  and in their cases :ORIGINAL is set to the original input to the parser
"
  (let [parser-fns (for [[parserid pred & specs] coll]
		     (let [[re re-keys postprocess-map]
			   (let [re (atom ""), ks (atom [:ORIGINAL]), postprocess (atom {})]
			     (dosync
			      (doseq [spec specs]
				(cond (string? spec)
				      (swap! re str spec)
				      
				      (vector? spec) ; [] [k] [k re] [k re postprocess-f]
				      (condp = (count spec)
					  1 (swap! ks conj (first spec)) ;; [k]
					  2 (let [[k r] spec]
					      (swap! ks conj k)
					      (swap! re str r))
					  3 (let [[k r f] spec]
					      (swap! ks conj k)
					      (swap! re str (r spec))
					      (swap! postprocess assoc k f))
					  {:error spec}))))
			     [@re @ks @postprocess])
			   re-pat (re-pattern re)]
		       (if (empty? postprocess-map)
			 (fn [s]
			   (let [parsed-row (zipmap re-keys (re-matches re-pat s))]
			     (when (pred parsed-row)
			       (assoc parsed-row :PARSER-ID parserid))))
			 (fn [s]
			   (let [parsed-row (into {} (map (fn [k v]
							    [k (if-let [f (postprocess-map k)]
								 (f v)
								 v)])
							  re-keys
							  (re-matches re-pat s)))]
			     (when (pred parsed-row)
			       (assoc parsed-row :PARSER-ID parserid)))))))]
    (fn [x]
      (or (first (filter :PARSER-ID (map #(% x) parser-fns)))
	  {:PARSER-ID nil :ORIGINAL x}))))

(defn re-parser* [re & kfs]
  (let [kfs (map #(or (and (vector? %) %) [% nil]) (cons [:matches boolean] kfs))]
    (fn [s]
      (dissoc (into {} (map (fn [[k f] v] [k (when k (if f (f v) v))])
			    kfs (re-matches re s))) nil))))


(defn filters [coll & preds-coll]
  (reduce #(filter %2 %1) coll preds-coll))

(defn replace-batch [s & replacements]
  (loop [coll (partition 2 replacements) s s]
    (if (empty? coll)
      s
      (let [[[x y] & tail] coll]
	(recur tail (.replaceAll (re-matcher ^Pattern x s) y))))))

(defn perm-to-num [collcoll ks]
  (reduce
   (fn [[kd md] [k-i m-i]]
     [(+ kd (* md k-i)) (* md m-i)])
   [0 1]
   (map vector ks (map count collcoll))))

(defn perm-from-num [collcoll kd]
  (second
   (reduce
    (fn [[kd ks] m-i]
      [(quot kd m-i) (conj ks (rem kd m-i))])
    [kd []]
    (map count collcoll))))

;; (perm-to-num   [[1 2 3] [1 1 3] [3 4 5 6]] [0 2 1]) -> [15 36]
;; (perm-from-num [[1 2 3] [1 1 3] [3 4 5 6]] 15) -> [0 2 1]

(defn unshy [x]
  (if (and (coll? x) (= (count x) 1)) (first x) x))

(defn make-instance [c & args]
  "A reflective and slow version of new"
  (clojure.lang.Reflector/invokeConstructor c (object-array args)))
