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

(defn uglify-name [^String s style]
  (condp = style
    :like_this (.replaceAll s "-" "_")
    :LIKE_THIS (.replaceAll (.toUpperCase s) "-" "_")
    :likethis (.replaceAll s "-" "")
    :LikeThis (apply str
                     (map string/capitalize (split-name s)))
    :likeThis (let [coll (split-name s)]
                (apply str
                       (first coll)
                       (map string/capitalize (rest coll))))
    s))

(defn uglify-name:test {:cli {}} []
  (doseq [style [:normal :like_this :LIKE_THIS :likethis :likeThis :LikeThis]]
    (println "\nStyle:" style)
    (doseq [s ["x-y"
               "happy-face"
               "-start-with-dash"
               "end-with-dash-"
               "double--dash"]]
      (println s "->" (uglify-name s style)))
    (println "----------------------------------------")))

(def js-uglify-name-map
  {:function :likeThis
   :variable :likeThis
   :class :LikeThis
   :enum :LikeThis
   :method :likeThis
   :constant :LIKE_THIS
   :namespace :likeThis
   :jsfile :likethis})

(defn js-uglify-name [^String s role]
  (if (re-find #"[a-zA-Z]" s)
    (uglify-name s (js-uglify-name-map role))
    s))

(defn js-uglify-name:test {:cli {}} []
  (doseq [role [:function :variable :class :enum :method :constant :namespace :jsfile]]
    (println "\nRole:" role)
    (doseq [s ["x-y"
               "happy-face"
               "-start-with-dash"
               "end-with-dash-"
               "double--dash"
               "foo.bar-baz"]]
      (println s "->" (js-uglify-name s role)))
    (println "----------------------------------------")))

;; TODO:
;;   - Implement while-do, switch-cases (condp-imp), try-catch-finally.
;;   - Turn into a stream of formattable pieces before turning to string.
;;   - Fix operator precedence properly.
;;   - Implement Type inference.
;;   - Take a spin at turning imperative to values passing.
;;   - Check rules in here (the ones we don't automatically satisfy).

;; Uglify should be actually run on formattable pieces not prior to that.

(def ^{:dynamic true} *infix-ambience* 24) ;; Normally between 1-18
(def ^{:dynamic true} *indent* 0) ;; Normally between 1-18

(def js-operator-map
  {'. {:prec 1 :assoc :left}
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

(defn line-break [& [n]]
  (concat (repeat (or n 1) "\n")
          (apply str (repeat *indent* " "))))

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
            [" "]
            (form->jsv arg))))

(defmethod list->jsv :no-args-or-unary [verb & body]
  (if-let [arg (first body)]
    (concat (form->jsv verb) [" "] (form->jsv arg))
    (form->jsv verb)))

(defmethod list->jsv :unary-right-bare-no-space [verb & body]
  (let [arg (first body)]
    (concat (form->jsv arg) (form->jsv verb))))

(defmethod list->jsv :if [verb & body]
  (let [[if-condition body-true body-false] body]
    (concat (form->jsv if-condition) [" ?"] [" "]
            (form->jsv body-true) [" :"] [" "]
            (form->jsv body-false))))

(defmethod list->jsv :do [verb & body]
  (mapcat identity (interpose [", "] (map form->jsv body))))

(defn list->jsv:do-imp [forms]
  (concat ["{"]
          (binding [*indent* (+ 2 *indent*)]
            (mapcat identity
                    (for [form forms]
                      (concat (line-break)
                              (form->jsv form)))))
          (line-break)
          ["}"]))

(defn list->jsv:do-imp-raw [forms]
  (mapcat identity
          (for [form forms]
            (concat (line-break)
                    (form->jsv form)))))

(defmethod list->jsv :do-imp [verb & body]
  (list->jsv:do-imp body))

(defn copyright-notice []
  ["// Copyright - 2012.  All rights reserved.\n"])

(defmethod list->jsv :__PROGRAM__ [verb & body]
  (concat (copyright-notice)
          (list->jsv:do-imp-raw body)))

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
      (concat ["for ("] (form->jsv a) ["; "] (form->jsv b) ["; "] (form->jsv c) [") "])

      (= b :in)
      (concat ["for ("] (form->jsv a) [" in "] (form->jsv c) [") "])

      (= b :index)
      (concat ["for ("]
              ["var "] (form->jsv a) [" = "] ["0"] ["; "]
              (form->jsv a) [" < "] (form->jsv c) ["."] ["length"] ["; "]
              ["++"] (form->jsv a) [") "])

      :else
      ;; (concat ["for ("]
      ;;         ["var "] (form->jsv b) [" = "] ["0"] ["; "]
      ;;         (form->jsv b) [" < "] (form->jsv c) ["."] ["length"] ["; "]
      ;;         ["++"] (form->jsv b) [") {"]
      ;;         (line-break)
      ;;         ["var "] (form->jsv a) " = " (form->jsv c) ["["] (form->jsv b) ["]"] [";"])
      ;; OR
      (concat ["for ("]
              ["var "] (form->jsv b) [" = "] ["0"] [", "]
              (form->jsv a) " = " (form->jsv c) ["[0]"] ["; "]
              (form->jsv b) [" < "] (form->jsv c) ["."] ["length"] ["; "]
              ["++"] (form->jsv b) [", "]
              (form->jsv a) " = " (form->jsv c) ["["] (form->jsv b) ["]"]  [") "])
      )
     (list->jsv:do-imp for-body))))

(defmethod list->jsv :var [verb & body]
  (concat
   ["var"]
   [" "]
   (mapcat identity
           (interpose [", "]
                      (for [[k v] (partition 2 body)]
                        (concat (form->jsv k) [" = "] (form->jsv v)))))
   [";"]))

(defmethod list->jsv :let [verb & body]
  (mapcat identity
          (for [[k v] (partition 2 (first body))]
            (concat  ["let "] (form->jsv k) [" = "] (form->jsv v) [";"]
                     (line-break)))))

(defmethod list->jsv :setq [verb & body]
  (if (= (count body) 2)
    (let [[k v] body]
      (concat (form->jsv k) [" = "] (form->jsv v) [";"]))
    (let [[k f v] body]
      (concat (form->jsv k) [" "] (form->jsv f) ["= "] (form->jsv v) [";"]))))

(defmethod list->jsv :when-imp [verb & body]
  (let [[if-cond & if-body] body]
    (concat
     ["if ("]
     (form->jsv if-cond) [")"] [" "]
     (list->jsv:do-imp if-body))))

(defmethod list->jsv :cond-imp [verb & body]
  (mapcat identity
          (for [[i [condition & forms]] (map vector (iterate inc 0) body)]
            (concat (cond (zero? i) ["if"]
                          (keyword? condition) [" else"]
                          :else [" else if"])
                    [" "] ["("] (form->jsv condition) [")"] [" "]
                    (list->jsv:do-imp forms)))))

(defmethod list->jsv :condp-imp [verb & body]
  (let [[pivot & clauses] body]
   (concat
    ["switch ("] (form->jsv pivot) [") {"]
    (binding [*indent* (+ 2 *indent*)]
      (mapcat identity
              (for [[case & case-body] clauses]
                (concat (line-break)
                        (if (keyword? case)
                          ["default:"]
                          (concat ["case "] (form->jsv case) [":"]))
                        (binding [*indent* (+ 2 *indent*)]
                          (mapcat identity
                                  (for [form case-body]
                                    (concat (line-break)
                                            (form->jsv form) [";"]))))))))
    (line-break)
    ["}"])))

(defmethod list->jsv :fn [verb & body]
  (let [[fn-args & fn-body] body]
    (concat ["function"] [" "] ["("]
            (mapcat identity
                    (interpose [", "] (map form->jsv fn-args)))
            [")"] [" "] (list->jsv:do-imp fn-body))))

(defmethod list->jsv :array-get [verb & body]
  (let [[array-name & indexes] body]
    (concat (form->jsv array-name)
            (mapcat identity
                    (for [index indexes]
                      (concat ["["] (form->jsv index) ["]"]))))))

(defmethod list->jsv :defn [verb & body]
  (let [[fn-name fn-args & fn-body] body]
    (concat
     (line-break)
     ["/**"]
     (mapcat identity
             (for [arg fn-args]
               (concat ["\n  * @param {"] [(get (meta arg) :tag)] ["} "]
                       (form->jsv arg) [" "] ;; Documentation should go here.
                       )))
     (when (= verb 'defn-)
       ["\n  * @private"])
     ["\n  */\n"]
     ["function"] [" "] (form->jsv fn-name) ["("]
     (mapcat identity (interpose [", "] (map form->jsv fn-args)))
     [")"] [" "]
     (list->jsv:do-imp fn-body))))

(defn list->jsv:infix [verb coll]
  (let [infix-verb (concat [" "] (form->jsv verb) [" "])
        operator-precedence (get (get js-operator-map verb) :prec)
        paren-wrap-maybe (fn [pred v]
                           (if pred
                             (concat ["("] v [")"])
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
          ["("]
          (mapcat identity (interpose [", "] (map form->jsv body)))
          [")"]))

(defmethod list->jsv :default [verb & body] ;; The same as :prefix
  (concat (form->jsv verb)
          ["("]
          (mapcat identity (interpose [", "] (map form->jsv body)))
          [")"]))

(defn form->jsv [form]
  ;; Should return a vector of items that can be combined by `apply'ing `str'.
  (cond
   (list? form)
   (apply list->jsv form)

   (map? form)
   (vec
    (concat ["{"]
            (mapcat identity
                    (interpose [", "]
                               (for [[k v] form]
                                 (concat (form->jsv k) [": "] (form->jsv v)))))
            ["}"]))

   (vector? form)
   (vec (concat ["["]
                (mapcat identity (interpose [", "] (map form->jsv form)))
                ["]"]))

   (or (symbol? form) (keyword? form))
   [(or (get (get js-operator-map form) :str)
        (js-uglify-name (name form) :variable))]

   (string? form)
   (apply str ["'"
               (string/escape form {\' "\"" \\ "\\\\"})
               "'"])

   :else
   [(pr-str form)]))

(defn form->js-str [form]
  (reduce str "" (form->jsv form)))

(defn to-js {:cli {}} []
  (println
   (form->js-str (read-string (str "(__PROGRAM__ " (slurp *in*) ")")))))

(defn form-to-js:test
  {:cli {:color Boolean :c :color}}
  []
  (when (clu/*opts* :color)
    (println "\033[31;1m#" (apply str (repeat 76 \-)) "#\033[0m"))
  (doseq [[i form]
          (map vector
               (iterate inc 1)
               ["This a string"
                'foo-bar-baz
                '(if true "-True-" "-False-")
                '(for [(def i 0) (<? i coll.length) (inc! i)] (println i))
                '(for [x :index (range 10)] (println x))
                '(for [x :in (range 10)] (println x))
                '(for [x xIndex (range 10)] (println x))
                '(fn []
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
                '(fn [x y] (+ x y z))
                '(fn [x] (* x x))
                '(fn [] (* 1 2))
                '(defn foo [^?Array= x  ;; Some info about x
                            ^String y]  ;; And some about y
                   ;; Docstring for foo
                   ;; More info
                   (* x y)
                   (return (* (pos 12)
                              (neg abcd)
                              (+ a b c))))
                '(defn- foo-private [x ^String y]
                   (* x y))

                '(foo (inc! a) (dec! b) (pos a) (neg b) (+ a b c) (% c d))
                '(do! (set! foo 4)
                      (set! foo bit-shift-left 2)
                      (<- foo bit-shift-right 2))
                '(fn []
                   (let [coll2 (range 10)
                         coll2 []
                         coll3 [1, 2, 3, 8]
                         map {:foo-bar foo :Alpha-beta bar}])
                   (for [x x-index coll]
                     (def bar (do (alert "Hello")
                                  (println "FOoOo")
                                  (* x x-index)))
                     (def barr 10, bazz 20, fooo 30)
                     (coll2.push bar)))
                ])]
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
