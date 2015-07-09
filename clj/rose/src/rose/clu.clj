;; Complettion for clove scripts
;; Needs clove context.
;; Copyright 2010-2012 and onwards Ali Honarvar honarvar@gmail.com

;; TODO: find a way to autoload this.

(ns rose.clu ;; command line utils
  (:require [clojure.string :as string]
            [clojure.java.shell]
            [rose.complete]
            [rose.file])) ;; only path

(declare ^{:dynamic true} *opts*)
(declare ^{:dynamic true} *opts-global*)

;; from clojure.contrib.with-ns:
(defmacro with-ns
  "Evaluates body in another namespace.  ns is either a namespace
  object or a symbol.  This makes it possible to define functions in
  namespaces other than the current one."
  [ns & body]
  `(binding [*ns* (the-ns ~ns)]
     ~@(map (fn [form] `(eval '~form)) body)))
;; ------------------------------------------------------------

(defn filter-prefix [prefix coll]
  (filter #(.startsWith (str %) prefix) coll))

(defn factor-prefix [coll]
  (loop [cmn nil coll coll]
    (if (or (empty? coll)
            (some empty? coll))
      [cmn coll]
      (let [c (first (first coll))]
        (if (some #(not= c %)
                  (map first (rest coll)))
          [cmn coll]
          (recur (str cmn c)
                 (map #(.substring % 1) coll)))))))

(defn- colls-string-join [l coll]
  (if (or (empty? l) (not (coll? coll)))
    (str coll)
    (let [[l1 & l-rest] l]
      (string/join l1
                   (seq (for [subcoll coll]
                          (colls-string-join l-rest subcoll)))))))

(defn- shell-escape-simple [s]
  (string/replace (str s) #"([	 \\\"()\[\]])" "\\\\$1"))

(def ^{:private true} ascii-color-map
  {:black 30 :k 30
   :red 31 :r 31
   :green 32 :g 32
   :yellow 33 :y 33
   :blue 34 :b 34
   :magenta 35 :m 35
   :cyan 36 :c 36
   :white 37 :w 37})

(def ^{:private true} ascii-color-extra-map
  {:bold 1 :b 1
   :dark 2 :d 2
   :underlined 4 :u 4
   :inverted 7 :v 7
   :hidden 8 :h 8
   :strikethrough 9 :strikethru 9 :s 9})

(defn ascii-color
  ([c s] (format "\033[%dm%s\033[m"
                 (or (ascii-color-map c) c) s))
  ([c cc s] (format "\033[%d;%dm%s\033[m"
                    (or (ascii-color-map c) c)
                    (or (ascii-color-extra-map cc) cc) s)))

(defn- path->ns [s]
  (.. (.replaceAll s "\\.clj$" "")
      (replace \_ \-)
      (replace \/ \.)))

(defn- path<-ns [n]
  (str (.. (.replaceAll (str (ns-name n))
                        "^bin\\." "")
           (replace \- \_)
           (replace \. \/))
       ".clj"))

(defn- str-drop-last [#^String s]
  (.substring s 0 (dec (.length s))))

(defn string-split-qe [s]
  "split a string while honouring escaping and a simple quotations.
Returns flags and a list in which tokens are placed in reverse order, i.e.,
head of the list is the last token.  Flags are: [escape, token, nil].
Hopefully flags should be [false true nil].  Otherwise you can decide what to
do to malformed or partial input."
  (reduce
   (let [add (fn [c [partial-token & rest-out]]
               (cons (str partial-token c) rest-out))
         set-sep (set " \t\r\n")
         set-quote (set "\"'")]
     (fn [[[flag-escape flag-token quote-char] out] c]
       (cond flag-escape
             [[false flag-token quote-char] (add c out)]

             (= c \\)
             [[true flag-token quote-char] out]

             (not flag-token)
             (cond (get set-sep c)   [[false false quote-char] out]
                   (get set-quote c) [[false true c] out] ; (add c out)
                   :else             [[false true quote-char] (add c out)])

             :else
             (if quote-char
               (if (= c quote-char)
                 [[false false nil] (cons nil out) ;; (cons nil (add c out))
                  ]
                 [[false flag-token quote-char] (add c out)])
               (cond (get set-sep c)   [[false false quote-char] (cons nil out)]
                     (get set-quote c) [[false true c] (cons nil out)]
                     :else             [[false flag-token quote-char]
                                        (add c out)])))))
   [[false false nil] nil]
   s))

;; (string-split-qe "abc def ")

(defn string-split-qe-forgiving [s]
  (let [[[flag-escape flag-token quote-char] out] (string-split-qe s)]
    (vec
     (reverse
      (cond ;; what if two or more conditions apply??
       ;; Conservative.  Probably just (rest out) instead of the `if' form is
       ;; correct.
       (not flag-token) (if (first out) out (rest out))
       ;; quote-char out
       ;; flag-escape out
       :else out)))))

;; (string-split-qe-forgiving "abc \\ def 'x\\ ")
;; (string/split "abc \\ def " #" +")

(defn line-parse
  ;; TODO: consider quotations, and shell escape.
  ([line] {:prev (string-split-qe-forgiving line)})
  ([line point]
     (let [l-before (string-split-qe-forgiving
                     (str (.substring line 0 point) "$"))
           l-after  (string-split-qe-forgiving
                     (str "^" (.substring line point)))]
       {:prev (drop-last l-before)
        :cur (str-drop-last (last l-before))
        :cur-residue (.substring (first l-after) 1)
        :extra (next l-after)})))

(defn- make-instance [c & args]
  "A reflective and slow version of new"
  (clojure.lang.Reflector/invokeConstructor c (object-array args)))

(defn ns-cli
  ([their-ns cli-key cli-default-key]
     ;; {:fns [(str k) k] :grammar [(str k) (cli-key (meta v))]}
     (let [{[[_ default-fn]] :default-fn,
            their-ns-cmds nil} (group-by
                                (fn [[k v]]
                                  ;; separate the default-fn from the rest
                                  (if (cli-default-key (meta v))
                                    :default-fn
                                    nil))
                                (filter #(cli-key (meta (second %)))
                                        (ns-publics their-ns)))]
       {:fns (into (if default-fn {nil default-fn} {})
                   (map (fn [[k v]] (vector (str k) v)) their-ns-cmds))

        :grammar
        (into (if default-fn
                (merge {nil (:arglists (meta default-fn))}
                       (cli-key (meta default-fn)))

                {})
              (map (fn [[k v]]
                     [(str k) (into {nil (:arglists (meta v))}
                                    (cli-key (meta v)))])
                   their-ns-cmds))}))

  ([their-ns cli-key]
     (let [their-ns-cmds (filter #(cli-key (meta (second %)))
                                 (ns-publics their-ns))]
       {:fns (into {}
                   (map (fn [[k v]] (vector (str k) v))
                        their-ns-cmds))

        :grammar (into {}
                       (map (fn [[k v]]
                              [(str k) (into {nil (:arglists (meta v))}
                                             (cli-key (meta v)))])
                            their-ns-cmds))}))

  ([their-ns]
     (ns-cli their-ns :cli :cli-default)))


(defn parse-args [grammar args]
  "parse args according to grammar
If parsing is not successful an error is printend on *err* and nil is
returned.  Otherwise, a map of parsed arguments is returned in which
the value for the key `nil' is a vector of residue arguments.
set: only members are valid,
fn: a predicates that decides what is valid
vector of 1 element: means the option can be repeated. check inside to what is valid
map: a sub-grammar

example:
  (parse-args {:x Boolean :y ^{:doc \"another on/off option\"} []
	       :z ^{:multi true} [Integer] ; collects a list of values
	       :from String :f :from ; an alias
	       :quad #{\"A\" \"B\" \"AB\" \"O\"} ; a set of possible values (only strings)
	       :num (fn [x] (> (Integer. x) 0))} ; a predicate
	      (.split \"input -xyz 0 -z 1 -z 2 +y --from=file --quad A --num 12 output\"
                      \" \"))
--> {:num \"12\", :quad \"A\", :from \"file\", :z (2 1 0), :y false, :x true, nil [\"input\" \"output\"]}

--opt=value, -x=value, and stringopt=value are accepted
-xy means -x -y, -xy=z means -x -y=z
"
  (loop [parsed-args {nil []} args args subcmd []]
    (if (empty? args)
      parsed-args

      (let [[first-arg next-args] [(first args) (next args)]]
        (if (= first-arg "--")
          (if (empty? subcmd)
            (update-in parsed-args [nil] into next-args)
            ;; subcmd must be kept a vector.
            (recur parsed-args next-args (vec (drop-last subcmd))))

          ;; else, it is either an option or a normal argument.
          (let [[_ beg-short opt-name-short opt-name-residue beg-long
                 opt-name-long opt-value]
                (re-matches
                 #"(?:(-|\+)([^=+-])([^=]+)?|(--)?([^=]+))(?:=(.+))?" first-arg)
                ;; `opt-name-residue' is used for cases like `-xy' which means
                ;; `-x -y'
                beg (or beg-long beg-short)
                opt-name (or opt-name-long opt-name-short)

                ;; resolve aliases (cases like: :l :list)
                [opt-key opt-spec]
                (loop [k (if beg (keyword opt-name) opt-name)
                       v (get (get-in grammar subcmd) k)]
                  (if (not (keyword? v))
                    [k v]
                    (recur v (get (get-in grammar subcmd) v))))]
            ;; Check if it is an option:
            (if opt-spec
              (let [opt-type (if (vector? opt-spec)
                               (cond (empty? opt-spec)
                                     Boolean

                                     :else
                                     (first opt-spec))
                               opt-spec)
                    update-fn
                    (if (:multi (meta opt-spec))
                      (fn [v]
                        (update-in parsed-args (conj subcmd opt-key) conj v))
                      (fn [v]
                        (assoc-in parsed-args (conj subcmd opt-key) v)))]
                (cond
                 (= opt-type Boolean)
                 (if opt-value
                   (.println *err* (str "Error: Boolean option `" opt-name "' "
                                        "does not accept an argument.\n"
                                        "       `" opt-value "' was supplied."))
                   (let [opt-value (not (= beg "+"))]
                     (recur (update-fn opt-value)
                            (if (empty? opt-name-residue)
                              next-args
                              (into [(str beg opt-name-residue)] next-args))
                            subcmd)))

                 (map? opt-type)
                 (recur (update-fn {}) next-args (conj subcmd opt-key))

                 ;; It is not Boolean or map, therefore it needs an arguemnt.
                 :else
                 (let [[opt-value residue-args]
                       (if opt-value
                         [opt-value next-args]
                         [(first next-args) (next next-args)])]
                   (cond
                    (= beg "+")
                    (.println *err* (str "Error: option `" opt-name "' started "
                                         "with `+'.  Only Boolean options are "
                                         "allowed to do so."))

                    (empty? opt-value)
                    (.println *err* (str "Error: No value supplied for option "
                                         "`" opt-name "'"))

                    (not-empty opt-name-residue)
                    (.println *err* (str "Error: Residue bundled options found "
                                         "for `" opt-name "' "
                                         "which requires a value."))

                    (class? opt-type)
                    (recur (update-fn (make-instance opt-type opt-value))
                           residue-args subcmd)

                    (or (set? opt-type)
                        (fn? opt-type))
                    (if (opt-type opt-value)
                      (recur (update-fn opt-value) residue-args subcmd)
                      (.println *err* (str "Error: Value for option `" opt-name
                                           "' cannot be `" opt-value "'.")))

                    :else
                    (.println *err* (str "Error: Option type for "
                                         "`" opt-name "' is " opt-type ", "
                                         "which is not allowed."))))))

              ;; So it is not an option, then it is a normal argument.
              ;; TODO: Check arglists to see if this arg is acceptable.
              (recur (update-in parsed-args (conj subcmd nil) (comp vec conj)
                                first-arg) next-args subcmd))))))))

(defn- key-to-opt [x]
  (if (keyword? x)
    (if (< 1 (count (name x)))
      (str "--" (name x))
      (str "-" (name x)))
    x))

(defn parse-args-partial [grammar args cur]
  (loop [parsed-args {nil []} args args subcmd []]
    (if (empty? args)
      {:cur subcmd
       :grammar (get-in grammar subcmd)
       :keys (map key-to-opt
                  (filter identity (keys (get-in grammar subcmd))))
       ;; :args (get-in grammar (conj subcmd nil))
       ;; :subargs (get-in parsed-args (conj subcmd nil))
       :arg-classes
       (let [cnt (count (get-in parsed-args (conj subcmd nil)))]
         ;; args parsed so far
         (set (filter
               identity
               (map (fn [arglist]
                      (:tag (meta
                             (get arglist
                                  ;; Position of first '& in arglist.
                                  (if-let [ampersand-pos
                                           (first
                                            (filter
                                             identity
                                             (map (fn [x n]
                                                    (and (= '& x) n))
                                                  arglist (iterate inc 0))))]
                                    (min cnt ampersand-pos)
                                    cnt)))))
                    (get-in grammar (conj subcmd nil))))))
       ;; Based on :args guess what types the next item can have
       }

      (let [[first-arg next-args] [(first args) (next args)]]
        (if (= first-arg "--")
          (if (empty? subcmd)
            (update-in parsed-args [nil] into next-args)
            ;; `subcmd' must be kept a vector.
            (recur parsed-args next-args (vec (drop-last subcmd))))

          ;; Else, it is either an option or a normal argument.
          (let [[_ beg-short opt-name-short opt-name-residue beg-long
                 opt-name-long opt-value]
                (re-matches
                 #"(?:(-|\+)([^=+-])([^=]+)?|(--)?([^=]+))(?:=(.+))?" first-arg)
                ;; `opt-name-residue' is used for cases like "-xy" which means
                ;; "-x -y"
                beg (or beg-long beg-short)
                opt-name (or opt-name-long opt-name-short)

                ;; resolve aliases (cases like: :l :list)
                [opt-key opt-spec]
                (loop [k (if beg (keyword opt-name) opt-name),
                       v (get (get-in grammar subcmd) k)]
                  (if (not (keyword? v))
                    [k v]
                    (recur v (get (get-in grammar subcmd) v))))]
            ;; Check if it is an option:
            (if opt-spec
              (let [opt-type (if (vector? opt-spec)
                               (cond (empty? opt-spec) Boolean
                                     :else (first opt-spec))
                               opt-spec)
                    update-fn (if (:multi (meta opt-spec))
                                (fn [v]
                                  (update-in parsed-args (conj subcmd opt-key)
                                             conj v))
                                (fn [v]
                                  (assoc-in parsed-args (conj subcmd opt-key)
                                            v)))]
                (cond
                 (= opt-type Boolean)
                 (if opt-value
                   {:error (str "Error: Boolean option `" opt-name "' "
                                "does not accept an argument.  "
                                "`" opt-value "' was supplied.")}
                   (let [opt-value (not (= beg "+"))]
                     (recur (update-fn opt-value)
                            (if (empty? opt-name-residue)
                              next-args
                              (into [(str beg opt-name-residue)] next-args))
                            subcmd)))

                 (map? opt-type)
                 (recur (update-fn {}) next-args (conj subcmd opt-key))

                 ;; It is not Boolean or map, therefore it needs an arguemnt.
                 :else
                 (let [[opt-value residue-args]
                       (if opt-value
                         [opt-value next-args]
                         [(first next-args) (next next-args)])]
                   (cond
                    (= beg "+")
                    {:error (str "Error: option `" opt-name "' started with "
                                 "\"+\".  Only Boolean options are allowed to "
                                 "do so.")}

                    (empty? opt-value)
                    {:error (str "Error: No value supplied for option "
                                 "`" opt-name "'")}

                    (not-empty opt-name-residue)
                    {:error (str "Error: Residue bundled options found for "
                                 "`" opt-name "' which requires a value.")}

                    (class? opt-type)
                    (recur (update-fn (make-instance opt-type opt-value))
                           residue-args subcmd)

                    (or (set? opt-type)
                        (fn? opt-type))
                    (if (opt-type opt-value)
                      (recur (update-fn opt-value) residue-args subcmd)
                      {:error (str "Error: Value for option `" opt-name "'"
                                   " cannot be `" opt-value "'.")})

                    :else
                    {:error (str "Error: Option type for `" opt-name "'"
                                 " is " opt-type ", which is not allowed.")}))))

              ;; So it is not an option, then it is a normal argument
              (recur (update-in parsed-args (conj subcmd nil) (comp vec conj)
                                first-arg) next-args subcmd))))))))

;; unused:
;; comp-type (*env* "COMP_TYPE")
;; comp-key  (*env* "COMP_KEY")

(defn main-dbg [& args]
  (.println *err* (str "args:" *command-line-args*))
  (.println *err* (str "comp-point: " (get *env* "COMP_POINT") ", "
                       "comp-line:  " (get *env* "COMP_LINE")
                       "comp-type:  " (get *env* "COMP_TYPE")
                       "comp-key:   " (get *env* "COMP_KEY"))))

(defn- completor-respond [coll]
  (println (string/join "\n" (sort (map shell-escape-simple coll)))))

;; TODO: complete according to the specified grammar
(defn cli-command-complete [line point]
  (let [{prev :prev cur :cur} (line-parse line point)]
    (if (empty? prev)
      (.println *err* "No args? We have freaked out!")

      (let [their-ns (symbol (str "bin." (path->ns (first prev))))]
        (require their-ns)

        ;; TODO:
        (let [cli (ns-cli their-ns)
              sub (parse-args-partial (merge {:help Boolean :h :help
                                              :version Boolean :v :version}
                                             (:grammar cli))
                                      (rest prev)
                                      cur)]
          (if (:error sub)
            (.println *err* (str "\n" (:error sub)))
            (first
             (filter not-empty
                     (list
                      (filter-prefix cur (:keys sub))
                      (mapcat #(cond
                                (class? %)
                                (.complete (make-instance % cur))

                                (symbol? %)
                                (.complete (make-instance
                                            (ns-resolve their-ns %) cur)))
                              (:arg-classes sub)))))))))))

(defn -main [& args]
  (if-let [point (get *env* "COMP_POINT")]
    ;; running as completer
    (completor-respond
     (cli-command-complete (get *env* "COMP_LINE")
                           (Integer/parseInt point)))

    ;; probably launching mode:
    (.println *err* "Sorry, COMP_POINT was not set.")))

(defn web-command-call [web-commands raw-cmd]
  (let [raw-args (:prev (line-parse raw-cmd))
        cmd (first raw-args)
        cmd-args (get (parse-args (:grammar web-commands) raw-args) cmd)]
    (if-let [cmd-fn (get (:fns web-commands) cmd)]
      (binding [*opts* (dissoc cmd-args nil)]
        (apply cmd-fn (get cmd-args nil)))
      (str cmd ": command not found"))))

(defn web-command-complete [web-commands their-ns raw-cmd point]
  (let [{prev :prev cur :cur extra :extra}
        (line-parse raw-cmd point)]
    (let [sub (parse-args-partial (:grammar web-commands) prev cur)]
      (or (:error sub)
          (let [[common-prefix options]
                (factor-prefix
                 (first
                  (filter not-empty
                          (list
                           (filter-prefix cur (:keys sub))
                           (mapcat #(cond (class? %)
                                          (.complete (make-instance % cur))

                                          (symbol? %)
                                          (.complete (make-instance
                                                      (ns-resolve their-ns %)
                                                      cur)))
                                   (:arg-classes sub))))))]
            {:prev prev
             :common (or common-prefix "")
             :options (filter not-empty options)
             :extra (or extra [])})))))

(defn sh [& args]
  "almost like clojure.java.shell, wrapped with *env* and *cwd*.
args are preprocessed: vectors are turned into paths,
and keywords are turned into longopts (:this --> --this)."
  (clojure.java.shell/with-sh-env *env*
    (clojure.java.shell/with-sh-dir *cwd*
      (apply clojure.java.shell/sh
             (map #(cond (vector? %) (apply rose.file/path %)
                         (= :< %) :in
                         (keyword? %) (key-to-opt %)
                         :else %) args)))))

;; ------------------------------------------------------------
;; this part is taken from the popen library
;; (https://bitbucket.org/tebeka/popen),
;; by Miki Tebeka <miki.tebeka@gmail.com>.
;; It is commented out because I am not sure what happens to
;; java.lang.Process if I run this.

(comment
  ;; require [clojure.java.io]
  (defn popen [args &{:keys [redirect dir]}]
    "Open a sub process, return the subprocess

  args - List of command line arguments
  :redirect - Redirect stderr to stdout
  :dir - Set initial directory"
    (-> (ProcessBuilder. args)
        (.directory (if dir
                      (clojure.java.io/file dir)
                      (java.io.File. *cwd)))
        (.redirectErrorStream (boolean redirect))
        (.start)))

  (defprotocol Popen
    (stdout [this] "Process standard output (read from)")
    (stdin [this] "Process standard input (write to)")
    (stderr [this] "Process standard error (read from)")
    (join [this] "Wait for process to terminate, return exit code")
    (exit-code [this] "Process exit code (will wait for termination)")
    (running? [this] "Return true if process still running")
    (kill [this] "Kill process"))

  (defn- exit-code- [p]
    (try
      (.exitValue p)
      (catch IllegalThreadStateException e)))

  (extend-type java.lang.Process
    Popen
    (stdout [this] (clojure.java.io/reader (.getInputStream this)))
    (stdin [this] (clojure.java.io/writer (.getOutputStream this)))
    (stderr [this] (clojure.java.io/reader (.getErrorStream this)))
    (join [this] (.waitFor this))
    (exit-code [this] (join this) (exit-code- this))
    (running? [this] (nil? (exit-code- this)))
    (kill [this] (.destroy this)))

  (defn popen* [args &{:keys [redirect dir]}]
    "Open a sub process, return the subprocess stdout

  args - List of command line arguments
  :redirect - Redirect stderr to stdout
  :dir - Set initial directory"
    (stdout (popen args :redirect redirect :dir dir))))
;; ------------------------------------------------------------

(comment
  (if (= (count prev) 1)
    ;; command not specified
    (completor-respond
     (filter-prefix cur (map #(:name (meta %))
                             (ns-interactive-fns current-ns))))

    ;; command specified
    (let [current-command (ns-resolve
                           current-ns (symbol (nth prev 1)))
          prev-args (drop 2 prev)
          an-arglist (first
                      (filter #(> (count %) (count prev-args))
                              (:arglists (meta current-command))))
          current-arg-tag (:tag
                           (meta (get an-arglist (count prev-args))))]
      ;; [an-arglist]
      (completor-respond
       (when an-arglist
         (if current-arg-tag
           (.complete (make-instance current-arg-tag cur))
           ;; new is apparently broken a bit

           ;; (rose.complete/complete
           ;;  (eval `(new ~(ns-resolve current-ns current-arg-tag) ~cur)))

           ["foo"] ;; TODO: use some default completion
           ))))))

(defn run-command-maybe-ns
  ([their-ns their-ns-path]
     ;; TODO: find a better condition to check if we are running a script
     (when (or
            (.endsWith (str "/" (get *env* "_"))
                       (str "/" their-ns-path))
            (.endsWith (str "/" (get *env* "_"))
                       "/make"))
       (let [cli (ns-cli their-ns)
             parsed-opts (parse-args (merge {:help Boolean :h :help
                                             :version Boolean :v :version}
                                            (:grammar cli))
                                     *command-line-args*)]
         ;; (println cli)
         ;; (println parsed-opts)
         (cond (:help parsed-opts)
               (do
                 (println "Usage:")
                 (doseq [cmd (filter (comp not keyword?) (keys (:grammar cli)))]
                   (println (format " %-15s" (or cmd ""))
                            (str (when-let [cmd-doc (:doc (meta (get (:fns cli)
                                                                     cmd)))]
                                   (ascii-color :y cmd-doc))))
                   (doseq [[k v] (sort-by #(str (first %))
                                          (get-in cli (if cmd
                                                        [:grammar cmd]
                                                        [:grammar])))]
                     (if k
                       (println (format " %-15s %s: %s%s" "" (key-to-opt k)
                                        (cond
                                         (keyword? v)
                                         (str "alias for " (key-to-opt v))

                                         (or (= Boolean v) (= [] v))
                                         "on/off option"

                                         :else
                                         v)
                                        (if-let [opt-doc (:doc (meta v))]
                                          (ascii-color :y (str " " opt-doc))
                                          "")))
                       (println (format " %-15s args: %s" ""
                                        (ascii-color :r (into (list) v))))))))

               :else
               (let [cli-fns (get cli :fns)
                     cmds (filter cli-fns (filter identity (keys parsed-opts)))]
                 ;; NOTE: We only run the default command when no command is
                 ;; specified.
                 (if (empty? cmds)
                   ;; Run default command
                   (binding [*opts* (dissoc parsed-opts nil)]
                     (when-let [f (get cli-fns nil)]
                       (apply f (get-in parsed-opts [nil]))))

                   ;; Run specified commands
                   (doseq [cmd cmds]
                     (binding [*opts* (dissoc (get-in parsed-opts [cmd]) nil)
                               *opts-global*
                               (select-keys parsed-opts
                                            (filter #(and % (not (string? %)))
                                                    (keys parsed-opts)))]
                       (when-let [f (get cli-fns cmd)]
                         (apply f (get-in parsed-opts [cmd nil])))))))))))
  ([their-ns]
     (run-command-maybe-ns their-ns (path<-ns their-ns))))

(defmacro run-command-maybe []
  `(~run-command-maybe-ns *ns*))

;; cruft
;; (println (.getCanonicalPath (java.io.File. *cwd* (first args))))
