(ns rose.cljml-tex
  (:require [clojure.string :as string]
            [clojure.walk :as walk]))

(def #^{:private true} print-tree)  ; forward declaration

(defn- escape-tex [s]
  (-> s
      (.replace "%" "\\%")
      (.replace "#" "\\#")
      (.replace "_" "\\_")
      (.replace "$" "\\$")))

(defn- escape-tex-2 [s]
  (-> s
      (.replace "%" "\\%")
      (.replace "$" "\\$")))

(defn interpose-peel [sep coll]
  (reduce #(concat %1 [sep] %2) coll))

(defn- print-attributes [m]
  (when (not-empty m)
    (print (str "["
                (string/join ","
                             (for [[k v] m]
                               (if k
                                 (str (name k) "=" (escape-tex v))
                                 ;; `nil' key does not get a `nil=' prefix.
                                 (escape-tex v))))
                "]"))))

;; types:
;; :after    f{x1}{x2}
;; :before   {f x1 x2}
;; :open-beg f x1 x2
;; :open-end x1 x2 f
;; :full     begin{f}\nx1\nx2\nend{f}
;; :peel
;; :delete

(def html-transform-map
  {:pre    [:full "lstlisting"] ;; {:env "listing"}
   :ul     [:full "itemize"]
   :ol     [:full "enumerate"]
   :eqn    [:eqn]

   ;; HTML representation no ready:
   :array   [:full "array"]
   :cases   [:full "cases"]
   :split   [:full "split"]
   :definition [:full "definition"] ;; Turn into dd, dt, etc.

   :em     [:before "\\em"]
   :i      [:before "\\em"]    ;; {:macro "em" :enclosure :before}
   :b      [:before "\\bf"]

   :size-4 [:before "\\tiny"]
   :size-3 [:before "\\scriptsize"]
   :size-2 [:before "\\footnotesize"]
   :size-1 [:before "\\small"]
   :size-0 [:before "\\normalsize"]
   :size+1 [:before "\\large"]
   :size+2 [:before "\\Large"]
   :size+3 [:before "\\LARGE"]
   :size+4 [:before "\\huge"]
   :size+5 [:before "\\Huge"]

   :h1     [:after "\\chapter"]  ;; {:macro "chapter" :enclosure :after}
   :h2     [:after "\\section"]
   :h3     [:after "\\subsection"]
   :h4     [:after "\\subsubsection"]
   :h5     [:after "\\subsubsubsection"] ;; or "paragraph"
   ;; :img [:full "figure"]	      ;; {:src :alt}
   :includegraphics [:after "\\includegraphics"]
   :footnote [:after "\\footnote"]
   :cite [:after "\\cite"]

   :code [:verb "\\lstinline" "||"] ;; {:macro "code" :enclosure :after :sep "||"}
   :$    [:verb "" "$$"]

   :hr [:open-end "\\\\\\hline"] ;; {:open }
   :br [:open-beg "\\newline"]
   :li [:open-beg "\\item"]

   ;; :tr   [:before "tr"]
   ;; :td   [:after "\\fbox"]
   ;; :th   [:before "\\bf"]
   :p    [:open-end "\n\n"]

   :tr [:peel]
   :th [:peel]
   :td [:peel]
   :tr-break [:open-beg "\\\\\n"]
   :tr-break-rule [:open-beg "\\\\\\hline\n"]
   :td-break [:open-beg " &"]
   :tabular [:full-tabular "tabular"]
   :table* [:full "table*"]
   :table [:full "table"]
   ;; :tbody  [:full "tabular"]
   ;; :table [:delete] ;; temporary
   ;; :table [:full "table"] ;; & \\
   :a [:anchor]
   ;; TODO:
   ;;   :a :href "protocol:url"
   ;;   :a :href "#local"    ;; Link to something within the same document.
   ;;   :a :name "foo:bar"   ;; should work for h., eqn, etc.
   ;;   :a :href :foo-bar  ;; Citation

   :body [:peel]
   ;; :body [:full "document"]
   :div [:div]
   :html [:peel]
   :head [:delete]
   })

(defn transformed-tag-name [x]
  (or (second (html-transform-map x)) (name x)))

(defmulti #^{:private true} print-tag
  (fn [tag attr content]
    (first (html-transform-map tag))))

(defmethod print-tag :comment [tag attr contents]
  (print "% ")
  (doseq [c contents] (print c)))

(defn print-full-env [env-name attr contents]
  (print (format "\\begin{%s}" env-name))
  (print-attributes (dissoc attr :name :class))
  (println)
  (when-let [label (attr :name)]
    (println (format "\\label{%s}" (escape-tex-2 label))))

  (if (seq contents)
    (doseq [c contents] (print-tree c))
    ;; empty tag:
    (println "% EMPTYTAG\n"))
  (print (format "\\end{%s}" env-name)))

(defmethod print-tag :full [tag attr contents]
  (print-full-env (transformed-tag-name tag) attr contents))

(defmethod print-tag :full-tabular [tag attr contents]
  (let [tag-name (transformed-tag-name tag)
        [table-format contents] contents]
    (print (str "\\begin{" tag-name "}"
                "{" table-format "}"))
    ;; (print-attributes attr)
    (if (seq contents)
      (do (doseq [c contents] (print-tree c))
          (println (format "\\end{%s}" tag-name)))
      ;; empty tag:
      (println (format "% EMPTYTAG\n\\end{%s}" tag-name)))))

(defmethod print-tag nil [tag attr contents]
  (let [tag-name (transformed-tag-name tag)]
    (print "\\begin{") (print tag-name) (print "}\n")
    (print-attributes attr)
    (if (seq contents)
      (do ;; not an empty tag
        (if (every? string? contents)
          ;; tag only contains strings:
          (do (doseq [c contents] (print-tree c))
              (print "\n\\end{") (print tag-name) (print "}"))
          ;; tag contains sub-tags:
          (do (doseq [c contents] (print-tree c))
              (print (format "\n\\end{%s}" tag-name)))))
      ;; empty tag:
      (print (format "nil-tag\n\\end{%s}" tag-name)))))

(defmethod print-tag :after [tag attr contents]
  (let [label (attr :name)
        attr (dissoc attr :name)]
    (print (transformed-tag-name tag))
    (print-attributes attr)
    (when (seq contents)
      (if (every? string? contents)
        ;; tag only contains strings:
        (do (doseq [c contents]
              (print "{") (print-tree c) (print "}")))
        ;; tag contains sub-tags:
        (do (doseq [c contents]
              (print "{") (print-tree c) (print "}")))))
    (when label
      (println (format "\\label{%s}" label)))))

(defmethod print-tag :verb [tag attr contents]
  (let [seps (get (html-transform-map tag) 2)]
    (print (transformed-tag-name tag))
    (print-attributes attr)
    (when (seq contents)
      (doseq [c contents]
        (print (first seps)) (print-tree c) (print (second seps))))))

(defmethod print-tag :before [tag attr contents]
  (print "{") (print (transformed-tag-name tag))
  (when (seq contents)
    (print " ")
    (if (every? string? contents)
      ;; tag only contains strings:
      (do (doseq [c contents] (print-tree c)))
      ;; tag contains sub-tags:
      (do (doseq [c contents] (print-tree c)))))
  (print "}"))

(defmethod print-tag :open-beg [tag attr contents]
  (print (transformed-tag-name tag))
  (print-attributes attr)
  (print " ")
  (when (seq contents)
    (if (every? string? contents)
      ;; tag only contains strings:
      (do (doseq [c contents] (print-tree c)))
      ;; tag contains sub-tags:
      (do (doseq [c contents] (print-tree c))))))

(defmethod print-tag :open-end [tag attr contents]
  (when (seq contents)
    (doseq [c contents] (print-tree c)))
  (print (transformed-tag-name tag)))

(defmethod print-tag :peel [tag attr contents]
  (doseq [c contents] (print-tree c)))


(defn transform-href [href contents]
  (cond
   (keyword? href)                      ; It must be a citation.
   (print (format "\\cite{%s}" (escape-tex-2 (name href))))

   (.startsWith href "#")        ; Link to something within the same document.
   (print (format "\\ref{%s}" (escape-tex-2 (.substring href 1))))

   ;; Link to an external file or other resource.
   (or (.startsWith href "http:")
       (.startsWith href "https:")
       (.startsWith href "file:")
       (.startsWith href "mailto:"))
   (do
     (print (str "\\href{" (escape-tex-2 href) "}{"))
     (print-tree contents)
     (print "}"))))

(defmethod print-tag :anchor [tag attr contents]
  (when attr
    (cond
     (attr :name) (do
                    (println (format "\\label{%s}" (escape-tex-2 (attr :name))))
                    (doseq [c contents] (print-tree c)))

     ;;   :a :href protocol:url
     ;;   :a :href #local
     (attr :href) (transform-href (attr :href) contents))))

(defmethod print-tag :eqn [tag attr contents]
  (if (attr :name)  ;; If :name is specified, create a numbered equation, else
    ;; a non-numbered.
    (print-full-env "align" attr contents)
    (print-full-env "align*" attr contents)))

(defmulti #^{:private true} print-div-tag
  (fn [main-class attr content]
    (keyword main-class)))

(defmethod print-div-tag :theorem [main-class attr contents]
  (print-full-env "theorem" attr contents))

(defmethod print-div-tag :lemma [main-class attr contents]
  (print-full-env "lemma" attr contents))

(defmethod print-div-tag :proof [main-class attr contents]
  (print-full-env "proof" attr contents))

(defmethod print-tag :div [tag attr contents]
  (if-let [special-class (when (get attr :class)
                           (some #{"theorem" "lemma" "proof"}
                                 (.split (attr :class) " ")))]
    (do (print-div-tag (keyword special-class) attr contents)
        (println))
    (doseq [c contents] (print-tree c))))

(defmethod print-tag :delete [tag attr contents]
  nil)

(defn replace-greek [^String s]
  (-> s
      (.replace "α" "\\alpha")
      (.replace "β" "\\beta")
      (.replace "μ" "\\mu")
      (.replace "μ" "\\mu")
      (.replace "θ" "\\theta")
      (.replace "γ" "\\gamma")
      (.replace "δ" "\\delta")
      (.replace "ε" "\\epsilon")
      (.replace "ζ" "\\zeta")
      (.replace "η" "\\eta")
      (.replace "ι" "\\iota")
      (.replace "κ" "\\kappa")
      (.replace "λ" "\\lambda")
      (.replace "ν" "\\nu")
      (.replace "ξ" "\\xi")
      ;; (.replace "ο" "o")
      (.replace "π" "\\pi")
      (.replace "ρ" "\\rho")
      (.replace "σ" "\\sigma")
      (.replace "τ" "\\tau")
      (.replace "υ" "\\upsilon")
      (.replace "φ" "\\phi")
      (.replace "χ" "\\chi")
      (.replace "ψ" "\\psi")
      (.replace "ω" "\\omega")

      (.replace "Α" "\\Alpha")
      (.replace "Β" "\\Beta")
      (.replace "Μ" "\\Mu")
      (.replace "Θ" "\\Theta")
      (.replace "Γ" "\\Gamma")
      (.replace "Δ" "\\Delta")
      (.replace "Ε" "\\Epsilon")
      (.replace "Ζ" "\\Zeta")
      (.replace "Η" "\\Eta")
      (.replace "Ι" "\\Iota")
      (.replace "Κ" "\\Kappa")
      (.replace "Λ" "\\Lambda")
      (.replace "Ν" "\\Nu")
      (.replace "Ξ" "\\Xi")
      ;; (.replace "Ο" "O")
      (.replace "Π" "\\Pi")
      (.replace "Ρ" "\\Rho")
      (.replace "Σ" "\\Sigma")
      (.replace "Τ" "\\Tau")
      (.replace "Υ" "\\Upsilon")
      (.replace "Φ" "\\Phi")
      (.replace "Χ" "\\Chi")
      (.replace "Ψ" "\\Psi")
      (.replace "Ω" "\\Omega")))

(defn replace-math [^String s]
  (-> s
      (.replace "≤" "\\leq")
      (.replace "≥" "\\geq")
      (.replace "≈" "\\approx")
      (.replace "≡" "\\equiv")
      (.replace "∈" "\\in")
      (.replace "⊂" "\\subset")
      (.replace "⊆" "\\subseteq")))

(defn print-tree [x]
  (condp instance? x

    clojure.lang.IPersistentVector (let [[tag & contents] x
                                         [attr content] (if (map? (first contents))
                                                          [(first contents) (rest contents)]
                                                          [{} contents])]
                                     (print-tag tag attr content))

    clojure.lang.ISeq (doseq [c x] (print-tree c))

    clojure.lang.Keyword (print-tag x {} nil)

    String (print
            (-> x
                ;; TODO: Setup a comprehensive translation map.
                (replace-greek)
                (replace-math)))

    ;; Default.
    (when x (print x))))

;; (defn assure [preds x]
;;   (when (every? #(% x) preds) x))

(defn sel-tag [x]
  (when (coll? x) (first x)))

(defn assure-map [x] (when (map? x) x))

(defn sel-attr
  ([x]
     (when (coll? x)
       (assure-map (second x))))
  ([k x]
     (get (sel-attr x) k)))

(defn sel-contents [x]
  (when (coll? x)
    (drop (if (map? (second x)) 2 1) x)))

;; (defn preprocess-comments [x]
;;   (if (coll? x)
;;     (if (= (first x) 'com)
;;       (str ";;" (next x) "\n")
;;       x)
;;     (str x)))

(defn preprocess [x]
  (cond

   ;; :table :tbody :tr :th/:td
   (= :table (sel-tag x))
   (let [table-tbody (first (sel-contents x))]
     (if (= :tbody (sel-tag table-tbody))
       (let [rows (sel-contents table-tbody)
             num-cols (count (sel-contents (first rows)))
             table-format (or (sel-attr :format x)
                              (string/join " " (repeat num-cols "l")))
             tr-break (if (get #{"-" "="} (sel-attr :vformat x))
                        [:tr-break-rule]
                        [:tr-break])]
         [:table*
          [:tabular table-format
           (interpose-peel tr-break
                           (map
                            #(interpose-peel [:td-break]
                                             (sel-contents %)) rows))]
          [:label (str "tab:" (sel-attr :id x))]
          [:caption (sel-attr :id x)]])
       x))

   ;; :img src alt
   (and (= :img (sel-tag x))
        (sel-attr :src x))
   (if (sel-attr :alt x)
     [:figure
      [:centering] [:includegraphics {:width "\\columnwidth"}
                    (sel-attr :src x)]
      [:label (sel-attr :name x)]
      [:caption (sel-attr :alt x)]]
     [:figure
      [:centering] [:includegraphics {:width "\\columnwidth"}
                    (sel-attr :src x)]])

   ;; :pre
   (= :pre (sel-tag x))
   `[~(sel-tag x)
     ~(merge (dissoc (sel-attr x) :xml:space :class)
             (if (sel-attr :class x)
               {:language (first (.split (sel-attr :class x) " "))} {}))
     ~@(sel-contents x)]

   ;; otherwise
   true x))

(defn transform-simple [arg]
  (with-out-str (print-tree arg)))

(defn transform [arg]
  (with-out-str
    (print-tree
     (walk/prewalk preprocess arg))))
