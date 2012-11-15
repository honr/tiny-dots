#!/usr/bin/env clove
;; | clojure

(ns bin.js
  (:require [rose.file :as rfile]
            [rose.clu :as clu]
            [rose.xml :as xml]
            [rose.gplot :as gplot]
            [clojure.string :as string]
            [clojure.pprint])
  (:import [java.io File]
           [org.joda.time
            DateTime Months Weeks MutableDateTime]))

(defn split-name [^String p]
  (loop [p p coll []]
    (let [n (.indexOf p "-")]
      (if (neg? n)
        (conj coll p)
        (recur (.substring p (inc n))
               (conj coll (.substring p 0 n)))))))

;; TODO: test.
(defn repair [value pred fixer & pred-fixers]
  ;; For each pred evaluating to true, try to "fix" the value by applying the
  ;; corresponding fixer.
  (if (pred value)
    (if-let [[more-pred more-fixer & more-pred-fixers] pred-fixers]
      (apply repair (fixer value)
             more-pred more-fixer more-pred-fixers)
      (fixer value))))

(defn js-uglify-name [^String s]
  (if-not (re-find #"[a-zA-Z]" s)
    s
    (if (and (.startsWith s "+") (.endsWith s "+"))
      ;; constants
      (.replace (.toUpperCase (.substring s 1 (dec (count s)))) \- \_)

      ;; -names-like-this- -> _namesLikeThis_
      (let [[beg coll] (split-with
                        empty?
                        (loop [p s beg [] coll []]
                          (let [n (.indexOf p "-")]
                            (if (neg? n)
                              (conj coll p)
                              (recur (.substring p (inc n))
                                     beg
                                     (conj coll (.substring p 0 n)))))))]
        (apply str
               (apply str (repeat (count beg) \_))
               (first coll)
               (map (fn [^String x]
                      (if (empty? x)
                        \_
                        (str (.toUpperCase (.substring x 0 1))
                             (.substring x 1))))
                    (rest coll)))))))

(defn js-uglify-name:test {:cli {}} []
  (doseq [s ["x-y"
             "+some-constant+"
             "+-some-ugly-constant-+"
             "Some-class"
             "some-method-"
             "-start-with-dash"
             "end-with-dash-"
             "double--dash"
             "foo.bar-baz"]]
    (println s "->" (js-uglify-name s)))
  (println "----------------------------------------"))

;; TODO:
;;   - Implement while-do, try-catch-finally.
;;   - Turn into a stream of formattable pieces before turning to string.
;;     <in progress>
;;   - Fix operator precedence properly.
;;     <revisit later>
;;   - Implement HM type inference.
;;   - Take a spin at turning imperative to value passing.
;;   - Check the rules we don't automatically satisfy.

(def ^{:dynamic true} *infix-ambience* 24) ;; Normally between 1-18
(def ^{:dynamic true} *line-prefix* "") ;; Used for indentation.
(def ^{:dynamic true} *cur-x* 0) ;; Also useful for indentation.

(defmacro indent [indentation & body]
  (let [line-prefix-appendage indentation]
    `(binding [*line-prefix* (str *line-prefix* ~line-prefix-appendage)]
       ~@body)))

(defn line-break
  ([] (concat ["\n"]
              [*line-prefix*]))
  ([n] (concat (repeat n "\n")
               [*line-prefix*])))

(defn suggest-string-break [^String s ^Long loc]
  (let [going-backward (max (.lastIndexOf s " " loc)
                            (.lastIndexOf s "	" loc))]
    (if-not (neg? going-backward)
      (inc going-backward)
      (let [going-forward (min (.indexOf s " " loc)
                               (.indexOf s "	" loc))]
        (if-not (neg? going-forward)
          (inc going-forward)
          (count s))))))

;; TODO: We should care about the current position in the line, too.
(defn string-fill [[^String beg ^String end ^String wrapper]
                   ^String s]
  (let [line-capacity (- 80 2 (count *line-prefix*))
        first-line-capacity (max 0 (- line-capacity *cur-x*))]
    (mapcat identity
            (interpose
             (concat [wrapper] (line-break))
             (loop [s s, coll [], capacity first-line-capacity]
               (cond
                (empty? s)
                coll

                (>= capacity (count s))
                (conj coll [beg s end])

                :else
                (let [break-point
                      (suggest-string-break s (- capacity 2))]
                  (recur (.substring s break-point)
                         (conj coll [beg (.substring s 0 break-point) end])
                         line-capacity))))))))

(def js-operator-map
  {'. {:prec 1 :assoc :left}
   'dot {:prec 1 :assoc :left :model :dot}
   'new! {:str "new" :prec 1 :assoc :right, :model :unary}

   '--funcall-- {:prec 2 :assoc :left}

   'inc! {:str "++" :prec 3 :assoc nil, :model :unary-right-bare-no-space}
   'dec! {:str "--" :prec 3 :assoc nil, :model :unary-right-bare-no-space}

   'bit-not {:str "~" :prec 4 :assoc :right, :model :unary-no-space}
   'not {:str "!" :prec 4 :assoc :right, :model :unary-no-space}
   'pos {:str "+" :prec 4 :assoc :right, :model :unary-no-space} ;; Is this used?
   'neg {:str "-" :prec 4 :assoc :right, :model :unary-no-space}

   'typeof {:prec 4 :assoc :right, :model :unary}
   'void {:prec 4 :assoc :right, :model :unary}
   'del! {:str "delete" :prec 4 :assoc :right, :model :unary}

   '/ {:prec 5 :assoc :left, :model :infix}
   '* {:prec 5 :assoc :left, :model :infix}
   '% {:prec 5 :assoc :left, :model :infix}

   '+ {:prec 6 :assoc :left, :model :infix}
   '- {:prec 6 :assoc :left, :model :infix}

   'bit-shift-left {:str "<<" :prec 7 :assoc :left, :model :infix}
   'bit-shift-right {:str ">>" :prec 7 :assoc :left, :model :infix}
   'bit-shift-right-z {:str ">>>" :prec 7 :assoc :left, :model :infix} ;; Zero-fill right shift

   '>? {:str ">" :prec 8 :assoc :left, :model :infix}
   '>=? {:str ">=" :prec 8 :assoc :left, :model :infix}
   '≥? {:str ">=" :prec 8 :assoc :left, :model :infix}
   '<? {:str "<" :prec 8 :assoc :left, :model :infix}
   '<=? {:str "<=" :prec 8 :assoc :left, :model :infix}
   '≤? {:str "<=" :prec 8 :assoc :left, :model :infix}
   'member? {:str "in" :prec 8 :assoc :left, :model :infix}
   'instance? {:str "instanceof" :prec 8 :assoc :left, :model :infix}

   '=? {:str "==" :prec 9 :assoc :left, :model :infix}
   'not=? {:str "!=" :prec 9 :assoc :left, :model :infix}
   '===? {:str "===" :prec 9 :assoc :left, :model :infix}
   'not===? {:str "!==" :prec 9 :assoc :left, :model :infix}

   'bit-and {:str "&" :prec 10 :assoc :left, :model :infix}

   'bit-xor {:str "^" :prec 11 :assoc :left, :model :infix}

   'bit-or {:str "|" :prec 12 :assoc :left, :model :infix}

   'and {:str "&&" :prec 13 :assoc :left, :model :infix}

   'or {:str "||" :prec 14 :assoc :left, :model :infix}

   ;; 'if {:str "?:" :prec 15 :assoc :right}

   'yield {:prec 16 :assoc :right, :model :unary}

   'set! {:prec 17 :assoc :right, :model :setq}
   '<- {:prec 17 :assoc :right, :model :setq}

   'do {:prec 18, :assoc :right, :model :do}

   ;; Non-operators:

   'throw {:assoc :right, :model :unary}

   'break {:model :no-args-or-unary}
   'continue {:model :no-args-or-unary}
   'return {:model :no-args-or-unary}
   'if {:model :if}
   'when {:model :when-imp}  ;; Imperative
   'cond {:model :cond-imp}  ;; Imperative
   'condp {:model :condp-imp}  ;; Imperative
   'for {:model :for-imp}        ;; Imperative
   'do! {:model :do-imp}
   'fn {:model :fn} ;; Not defn. This is lambda.
   'ns {:model :ns}
   'defn {:model :defn}
   'defn- {:model :defn}
   'get {:model :unary}  ;; ???
   'set {:model :unary}  ;; ???

   'let {:model :let}
   'def {:model :var}
   'aget {:model :array-get}
   'debugger {:model :no-args}
   '__PROGRAM__ {:model :__PROGRAM__}
   ;; :EOS ";\n" ;; End of Statement
   })

(def jsv:space [" "])
(def jsv:comma-space [", "])
(def jsv:semicolon-space ["; "])
(def jsv:semicolon [";"])
(def jsv:dot ["."])
(def jsv:assign-to [" = "])
(def jsv:paren-open ["("])
(def jsv:paren-close [")"])
(def jsv:square-open ["["])
(def jsv:square-close ["]"])
(def jsv:curly-open ["{"])
(def jsv:curly-close ["}"])
(def jsv:string-open ["'"])
(def jsv:string-close ["'"])

(defmulti list->jsv
  (fn [verb & _] (get (get js-operator-map verb) :model)))

(declare form->jsv)

(defmethod list->jsv :no-args [verb & body]
  (form->jsv verb))

(defmethod list->jsv :unary-no-space [verb & body]
  (let [arg (first body)]
    (concat (form->jsv verb)
            (form->jsv arg))))

(defmethod list->jsv :unary [verb & body]
  (let [arg (first body)]
    (concat (form->jsv verb)
            jsv:space
            (form->jsv arg))))

(defmethod list->jsv :no-args-or-unary [verb & body]
  (if-let [arg (first body)]
    (concat (form->jsv verb) jsv:space (form->jsv arg))
    (form->jsv verb)))

(defmethod list->jsv :unary-right-bare-no-space [verb & body]
  (let [arg (first body)]
    (concat (form->jsv arg) (form->jsv verb))))

(defmethod list->jsv :if [verb & body]
  (let [[if-condition body-true body-false] body]
    (concat (form->jsv if-condition) [" ?"] jsv:space
            (form->jsv body-true) [" :"] jsv:space
            (form->jsv body-false))))

(defmethod list->jsv :do [verb & body]
  (mapcat identity (interpose jsv:comma-space (map form->jsv body))))

(defmethod list->jsv :dot [verb & body]
  (mapcat identity (interpose jsv:dot (map form->jsv body))))

(defn list->jsv:do-imp [forms]
  (concat jsv:curly-open
          (indent "  "
                  (mapcat identity
                          (for [form forms]
                            (concat (line-break)
                                    (form->jsv form)))))
          (line-break)
          jsv:curly-close))

(defn list->jsv:do-imp-raw [forms]
  (mapcat identity
          (for [form forms]
            (concat (line-break)
                    (form->jsv form)))))

(defmethod list->jsv :do-imp [verb & body]
  (list->jsv:do-imp body))

(defmethod list->jsv :__PROGRAM__ [verb & body]
  (mapcat form->jsv body))

(defmethod list->jsv :for-imp [verb & body]
  ;; (for [x :in coll] ...) ->
  ;; for (x in coll) ...

  ;; (for [x xIndex coll] ...) ->
  ;; for (var xIndex = 0; xIndex < coll.length; ++xIndex) ...
  ;;   var x = coll[xIndex]; ...
  ;; OR
  ;; for (var xIndex = 0, x = coll[0]; xIndex < coll.length; ++i, x=coll[xIndex]) ...

  ;; (for [i :index coll] ...) ->
  ;; for (var i = 0; i < coll.length; ++i) ...

  (let [[for-header & for-body] body
        [a b c] for-header]
    (concat
     (cond
      (not (or (symbol? a) (symbol? b) (symbol? c)))
      (concat ["for ("] (form->jsv a) jsv:semicolon-space (form->jsv b) jsv:semicolon-space (form->jsv c) [") "])

      (= b :in)
      (concat ["for ("] (form->jsv a) [" in "] (form->jsv c) [") "])

      (= b :index)
      (concat ["for ("]
              ["var "] (form->jsv a) jsv:assign-to ["0"] jsv:semicolon-space
              (form->jsv a) [" < "] (form->jsv c) jsv:dot ["length"] jsv:semicolon-space
              ["++"] (form->jsv a) [") "])

      :else
      ;; (concat ["for ("]
      ;;         ["var "] (form->jsv b) jsv:assign-to ["0"] jsv:semicolon-space
      ;;         (form->jsv b) [" < "] (form->jsv c) jsv:dot ["length"] jsv:semicolon-space
      ;;         ["++"] (form->jsv b) [") {"]
      ;;         (line-break)
      ;;         ["var "] (form->jsv a) " = " (form->jsv c) jsv:square-open (form->jsv b) jsv:square-close jsv:semicolon)
      ;; OR
      (concat ["for ("]
              ["var "] (form->jsv b) jsv:assign-to ["0"] jsv:comma-space
              (form->jsv a) " = " (form->jsv c) ["[0]"] jsv:semicolon-space
              (form->jsv b) [" < "] (form->jsv c) jsv:dot ["length"] jsv:semicolon-space
              ["++"] (form->jsv b) jsv:comma-space
              (form->jsv a) " = " (form->jsv c) jsv:square-open (form->jsv b) jsv:square-close  [") "])
      )
     (list->jsv:do-imp for-body))))

(defmethod list->jsv :var [verb & body]
  (concat
   ["var"]
   jsv:space
   (mapcat identity
           (interpose jsv:comma-space
                      (for [[k v] (partition 2 body)]
                        (concat (form->jsv k) jsv:assign-to (form->jsv v)))))
   jsv:semicolon))

(defmethod list->jsv :let [verb & body]
  (mapcat identity
          (for [[k v] (partition 2 (first body))]
            (concat  ["let "] (form->jsv k) jsv:assign-to (form->jsv v) jsv:semicolon
                     (line-break)))))

(defmethod list->jsv :setq [verb & body]
  (if (= (count body) 2)
    (let [[k v] body]
      (concat (form->jsv k) jsv:assign-to (form->jsv v) jsv:semicolon))
    (let [[k f v] body]
      (concat (form->jsv k) jsv:space (form->jsv f) ["= "] (form->jsv v) jsv:semicolon))))

(defmethod list->jsv :when-imp [verb & body]
  (let [[if-cond & if-body] body]
    (concat
     ["if ("]
     (form->jsv if-cond) jsv:paren-close jsv:space
     (list->jsv:do-imp if-body))))

(defmethod list->jsv :cond-imp [verb & body]
  (mapcat identity
          (for [[i [condition & forms]] (map vector (iterate inc 0) body)]
            (concat (cond (zero? i) ["if"]
                          (keyword? condition) [" else"]
                          :else [" else if"])
                    jsv:space jsv:paren-open (form->jsv condition) jsv:paren-close jsv:space
                    (list->jsv:do-imp forms)))))

(defmethod list->jsv :condp-imp [verb & body]
  (let [[pivot & clauses] body]
   (concat
    ["switch ("] (form->jsv pivot) [") {"]
    (indent "  "
            (mapcat identity
                    (for [[case & case-body] clauses]
                      (concat (line-break)
                              (if (keyword? case)
                                ["default:"]
                                (concat ["case "] (form->jsv case) [":"]))
                              (indent "  "
                                      (mapcat identity
                                              (for [form case-body]
                                                (concat (line-break)
                                                        (form->jsv form) jsv:semicolon))))))))
    (line-break)
    jsv:curly-close)))

(defmethod list->jsv :fn [verb & body]
  (let [[fn-args & fn-body] body]
    (concat ["function"] jsv:space jsv:paren-open
            (mapcat identity
                    (interpose jsv:comma-space (map form->jsv fn-args)))
            jsv:paren-close jsv:space (list->jsv:do-imp fn-body))))

(defmethod list->jsv :ns [verb & body]
  (let [[ns-name &
         [{copyright :copyright
           author :author
           deps :require}
          overview-doc]] body]
    (concat [(format "// Copyright %s. All Rights Reserved." copyright)]
            (line-break)
            (when overview-doc
              (concat
               (line-break)
               ["/**"] (line-break)
               [" * "] [(str "@fileoverview " overview-doc)] (line-break)
               [" * "] [(str "@auther " author)] (line-break)
               [" */"] (line-break)))

            (line-break)
            ["goog.provide(" (str "'"
                                  (apply str (form->jsv ns-name))
                                  "'") ")" ";"]
            (line-break)
            (mapcat identity
                    (for [dep deps]
                      (concat (line-break)
                              ["goog.require("
                               (str "'"
                                    (apply str (form->jsv dep))
                                    "'")
                               ")" ";"])))
            (line-break))))

(defmethod list->jsv :array-get [verb & body]
  (let [[array-name & indexes] body]
    (concat (form->jsv array-name)
            (mapcat identity
                    (for [index indexes]
                      (concat jsv:square-open (form->jsv index) jsv:square-close))))))

(defn type->jsv [args]
  (cond
   (list? args)
   (let [[parent & children] args]
     (concat
      [(type->jsv parent)] [".<"]
      (mapcat identity
              (interpose jsv:comma-space (map type->jsv children)))
      [">"]))

   (vector? args)
   (mapcat identity
           (interpose ["|"] (map type->jsv args)))

   (symbol? args)
   (js-uglify-name (name args))

   :else
   (str args)))

(defn extract-args [args]
  (loop [args args args-with-meta []]
    (if (empty? args)
      args-with-meta
      (let [[f & r] args]
        (if (= f '•)
          (let [[arg arg-type arg-doc & remaining-r] r]
            (recur remaining-r
                   (conj args-with-meta
                         {:name arg :type arg-type :doc arg-doc})))
          (recur r
                 (conj args-with-meta
                       {:name f})))))))

(defmethod list->jsv :defn [verb & body]
  (let [[fn-name fn-args & fn-body] body
        args-with-meta (extract-args fn-args)]

    (concat
     (line-break)
     ["/**"]
     (mapcat identity
             (for [arg args-with-meta]
               (concat (line-break)
                       [" * @param {"] (type->jsv (get arg :type)) jsv:curly-close jsv:space
                       (form->jsv (get arg :name)) jsv:space
                       (indent "    "
                               ;; TODO: wrap around.
                               [(get arg :doc)]))))
     (when (= verb 'defn-)
       (concat (line-break) [" * @private"]))
     (line-break) [" */"]
     (line-break)
     [(format "function %s(" (apply str (form->jsv fn-name)))]
     (mapcat identity (interpose jsv:comma-space (map form->jsv
                                             (map :name args-with-meta))))
     [") "]
     (list->jsv:do-imp fn-body))))

(defn list->jsv:infix [verb coll]
  (let [infix-verb (concat jsv:space (form->jsv verb) jsv:space)
        operator-precedence (get (get js-operator-map verb) :prec)
        paren-wrap-maybe (fn [pred v]
                           (if pred
                             (concat jsv:paren-open v jsv:paren-close)
                             v))]
    (paren-wrap-maybe (< *infix-ambience* operator-precedence)
                      (binding [*infix-ambience* operator-precedence]
                        (mapcat identity
                                (interpose infix-verb
                                           (map form->jsv coll)))))))

(defmethod list->jsv :infix [verb & body]
  (list->jsv:infix verb body))

(defmethod list->jsv :infix-or-unary [verb & body]
  (if (not-empty (rest body)) ;; Has more than one element.
    (list->jsv:infix verb body)

    ;; Unary:
    (concat (form->jsv verb)
            (form->jsv (first body)))))

(defmethod list->jsv :prefix [verb & body]
  (concat (form->jsv verb)
          jsv:paren-open
          (mapcat identity (interpose jsv:comma-space (map form->jsv body)))
          jsv:paren-close))

(defmethod list->jsv :default [verb & body] ;; The same as :prefix
  (concat (form->jsv verb)
          jsv:paren-open
          (mapcat identity (interpose jsv:comma-space (map form->jsv body)))
          jsv:paren-close))

(defn form->jsv [form]
  ;; Should return a vector of items that can be combined by `apply'ing `str'.
  (cond
   (list? form)
   (apply list->jsv form)

   (map? form)
   (concat jsv:curly-open
           (mapcat identity
                   (interpose jsv:comma-space
                              (for [[k v] form]
                                (concat (form->jsv k) [": "] (form->jsv v)))))
           jsv:curly-close)

   (vector? form)
   (concat jsv:square-open
           (mapcat identity (interpose jsv:comma-space (map form->jsv form)))
           jsv:square-close)

   (or (symbol? form) (keyword? form))
   [(or (get (js-operator-map form) :str)
        (js-uglify-name (name form)))]

   (string? form)
   (string-fill
    ["'" "'" " +"]
    (string/escape form {\' "\"", \\ "\\\\"}))

   :else
   [(pr-str form)]))

(defn form->js-str [form]
  (reduce str "" (form->jsv form)))

(defn to-js {:cli {}} []
  (println
   (form->js-str (read-string (str "(__PROGRAM__ " (slurp *in*) ")")))))

(defn form-to-js:test
  {:cli {:color Boolean :c :color
         :test-name String :t :test-name}}
  []
  (when (clu/*opts* :color)
    (println "\033[31;1m#" (apply str (repeat 76 \-)) "#\033[0m"))
  (doseq [[i form]
          (map vector
               (iterate inc 1)
               (if-let [test-name (keyword
                                   (clu/*opts* :test-name))]
                 (get (read-string (slurp (str *home* "/.test/bin/js.clj")))
                      test-name)
                 ['(defn foo [• x (String)
                              "This parameter has a particularly long document string which should be wrapped around."]
                     (def coll (range 10))
                     (for [x x-index coll]
                       (when (=? (* x x) 9)
                         (println x)
                         (break))
                       (cond ((=? (* x x) 9)
                              (break))
                             ((=? (* x x) 81)
                              (break))
                             (:else
                              (println x)))
                       (condp (* x x)
                           (9 (break))
                         (81 (break))
                         (:else (println x) (break))))
                     (for [i :index coll]
                       (def x (aget coll i))
                       (continue :some-label)
                       (debugger)
                       (throw exception)
                       (return 41)))
                  ]))]
    (if (clu/*opts* :color)
      (println (format (if (clu/*opts* :color)
                         "\033[36;1m// [%02d]\033[0m\n\033[34;1m%s\033[0m ->"
                         "// [%02d] %s ->")
                       i
                       (with-out-str
                         (clojure.pprint/pprint form)))))
    (println (form->js-str form))
    (println "----------------------------------------\n")))

(clu/run-command-maybe-ns *ns* "js.clj")
