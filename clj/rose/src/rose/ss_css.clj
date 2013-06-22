(ns rose.ss-css
  (:require [clojure.string :as string]))

;; TODO: evaluate all symbols!
;; TODO: allow inserting comments and section headers.
;; TODO: allow multiple key-values, decidable by value as well as by key.
;; TODO: merge style.clj, style/foo.clj, style/bar.clj into one.  use `foo'
;;       and `bar' as namespaces.  style.clj should contain generic and/or
;;       classless rules.

(defprotocol AbelianGroupIface
  "Operations for an Abelian Group"
  (add [m value])
  (mul [m coeff]))

(deftype NumberWithUnit
    [num unit]
  Object
  (toString [_]
    (if unit
      (str num (name unit))
      (str num)))
  AbelianGroupIface
  (mul [m coeff]
    (NumberWithUnit. (* coeff num) unit)))

(defn rulize [body]
  (loop [body body cur [] rules [] defs []]
    (if (empty? body)
      {:defs defs :rules rules}
      (let [[x & body] body]
        (cond
         (seq? x) (recur body [] rules (conj defs x))

         (string? x) (recur body (conj cur x) rules defs)

         (map? x) (recur body [] (conj rules [cur x]) defs))))))

(defn make-context [defs]
  (into {}
        (for [row defs]
          (let [[f & row-body] row]
            (condp = f
              'def
             [(first row-body) (second row-body)]

             'ns
             [:ns (first row-body)]

             nil
              ;; We do nothing for the rest of them yet!
             )))))

(defn hex-to-vector [s num-elements]
  (let [s (if (.startsWith s "#") (.substring s 1) s)
        digits-per-val (/ (count s) num-elements)
        normalizing-factor (/ (+ -1 (apply * (repeat digits-per-val 16))))]

    (for [beg (range 0 (count s) digits-per-val)]
      (* normalizing-factor
         (Integer/valueOf (.substring s beg (+ beg digits-per-val))
                          16)))))

(defn rgbhexstr-to-hsl [rgb]  ;; r, g, b: [00, FF]
  (let [[r g b] (hex-to-vector rgb 3)
        alpha (- (* 2 r) g b)
        beta (- g b)
        M (max r g b)
        m (min r g b)
        C (- M m)]
    [;; Hue:
     (/ (cond
         (zero? C) 0
         (= M r) (mod (/ (- g b) C) 6)
         (= M g) (+ 2 (/ (- b r) C))
         (= M b) (+ 4 (/ (- r g) C)))
        6)

     ;; Saturation:
     (if (zero? C) 0
         (/ C (- 1 (Math/abs (+ M m -1.0)))))

     ;; Lightness:
     (* 0.5 (+ M m))]))

(def known-rgb-colors
  [;; Indented ones better hold to their Hue-Sat at Lum = 0.5.
   ["aliceblue" "#f0f8ff"]
   ["antiquewhite" "#FAEBD7"]
    ["aqua" "#00FFFF"]
    ["aquamarine" "#7FFFD4"]
   ["azure" "#F0FFFF"]
   ["beige" "#F5F5DC"]
   ["bisque" "#FFE4C4"]
    ["black" "#000000"]
   ["blanchedalmond" "#FFEBCD"]
    ["blue" "#0000FF"]
    ["blueviolet" "#8A2BE2"]
    ["brown" "#A52A2A"]
   ["burlywood" "#DEB887"]
    ["cadetblue" "#5F9EA0"]
    ["chartreuse" "#7FFF00"]
    ["chocolate" "#D2691E"]
   ["coral" "#FF7F50"]
   ["cornflowerblue" "#6495ED"]
   ["cornsilk" "#FFF8DC"]
    ["crimson" "#DC143C"]
    ["cyan" "#00FFFF"]
   ["darkblue" "#00008B"]
   ["darkcyan" "#008B8B"]
   ["darkgoldenrod" "#B8860B"]
   ["darkgray" "#A9A9A9"]
   ["darkgreen" "#006400"]
   ["darkgrey" "#A9A9A9"]
   ["darkkhaki" "#BDB76B"]
   ["darkmagenta" "#8B008B"]
   ["darkolivegreen" "#556B2F"]
    ["darkorange" "#FF8C00"]
    ["darkorchid" "#9932CC"]
   ["darkred" "#8B0000"]
   ["darksalmon" "#E9967A"]
   ["darkseagreen" "#8FBC8F"]
    ["darkslateblue" "#483D8B"]
   ["darkslategray" "#2F4F4F"]
   ["darkslategrey" "#2F4F4F"]
   ["darkturquoise" "#00CED1"]
   ["darkviolet" "#9400D3"]
    ["deeppink" "#FF1493"]
    ["deepskyblue" "#00BFFF"]
   ["dimgray" "#696969"]
   ["dimgrey" "#696969"]
    ["dodgerblue" "#1E90FF"]
    ["firebrick" "#B22222"]
   ["floralwhite" "#FFFAF0"]
   ["forestgreen" "#228B22"]
    ["fuchsia" "#FF00FF"]
   ["gainsboro" "#DCDCDC"]
   ["ghostwhite" "#F8F8FF"]
    ["gold" "#FFD700"]
    ["goldenrod" "#DAA520"]
    ["gray" "#808080"]
    ["green" "#008000"]
    ["greenyellow" "#ADFF2F"]
    ["grey" "#808080"]
   ["honeydew" "#F0FFF0"]
    ["hotpink" "#FF69B4"]
    ["indianred" "#CD5C5C"]
    ["indigo" "#4B0082"]
   ["ivory" "#FFFFF0"]
    ["khaki" "#F0E68C"]
   ["lavender" "#E6E6FA"]
   ["lavenderblush" "#FFF0F5"]
    ["lawngreen" "#7CFC00"]
   ["lemonchiffon" "#FFFACD"]
   ["lightblue" "#ADD8E6"]
   ["lightcoral" "#F08080"]
   ["lightcyan" "#E0FFFF"]
   ["lightgoldenrodyellow" "#FAFAD2"]
   ["lightgray" "#D3D3D3"]
   ["lightgreen" "#90EE90"]
   ["lightgrey" "#D3D3D3"]
   ["lightpink" "#FFB6C1"]
   ["lightsalmon" "#FFA07A"]
   ["lightseagreen" "#20B2AA"]
   ["lightskyblue" "#87CEFA"]
   ["lightslategray" "#778899"]
   ["lightslategrey" "#778899"]
   ["lightsteelblue" "#B0C4DE"]
   ["lightyellow" "#FFFFE0"]
    ["lime" "#00FF00"]
    ["limegreen" "#32CD32"]
   ["linen" "#FAF0E6"]
    ["magenta" "#FF00FF"]
   ["maroon" "#800000"]
    ["mediumaquamarine" "#66CDAA"]
    ["mediumblue" "#0000CD"]
    ["mediumorchid" "#BA55D3"]
   ["mediumpurple" "#9370DB"]
   ["mediumseagreen" "#3CB371"]
   ["mediumslateblue" "#7B68EE"]
   ["mediumspringgreen" "#00FA9A"]
   ["mediumturquoise" "#48D1CC"]
    ["mediumvioletred" "#C71585"]
   ["midnightblue" "#191970"]
   ["mintcream" "#F5FFFA"]
   ["mistyrose" "#FFE4E1"]
   ["moccasin" "#FFE4B5"]
   ["navajowhite" "#FFDEAD"]
   ["navy" "#000080"]
   ["oldlace" "#FDF5E6"]
    ["olive" "#808000"]
    ["olivedrab" "#6B8E23"]
    ["orange" "#FFA500"]
    ["orangered" "#FF4500"]
    ["orchid" "#DA70D6"]
   ["palegoldenrod" "#EEE8AA"]
   ["palegreen" "#98FB98"]
   ["paleturquoise" "#AFEEEE"]
    ["palevioletred" "#DB7093"]
   ["papayawhip" "#FFEFD5"]
   ["peachpuff" "#FFDAB9"]
    ["peru" "#CD853F"]
   ["pink" "#FFC0CB"]
   ["plum" "#DDA0DD"]
   ["powderblue" "#B0E0E6"]
   ["purple" "#800080"]
    ["red" "#FF0000"]
    ["rosybrown" "#BC8F8F"]
    ["royalblue" "#4169E1"]
   ["saddlebrown" "#8B4513"]
    ["salmon" "#FA8072"]
   ["sandybrown" "#F4A460"]
    ["seagreen" "#2E8B57"]
   ["seashell" "#FFF5EE"]
    ["sienna" "#A0522D"]
   ["silver" "#C0C0C0"]
    ["skyblue" "#87CEEB"]
    ["slateblue" "#6A5ACD"]
   ["slategray" "#708090"]
   ["slategrey" "#708090"]
   ["snow" "#FFFAFA"]
    ["springgreen" "#00FF7F"]
    ["steelblue" "#4682B4"]
    ["tan" "#D2B48C"]
    ["teal" "#008080"]
   ["thistle" "#D8BFD8"]
    ["tomato" "#FF6347"]
    ["turquoise" "#40E0D0"]
    ["violet" "#EE82EE"]
    ["wheat" "#F5DEB3"]
    ["white" "#FFFFFF"]
   ["whitesmoke" "#F5F5F5"]
    ["yellow" "#FFFF00"]
    ["yellowgreen" "#9ACD32"]])

;; (println known-rgb-colors)

(def known-colors-hue-sat
  (into {}
        (for [[color-name rgbhex] known-rgb-colors]
          [color-name
           ;; drop the L of HSL.
           (take 2 (rgbhexstr-to-hsl rgbhex))])))

(defn lossy-float-str [x]
  (let [s (format "%.2f" x)]
    (if (< -1 x 1)
      (.substring s 1))))

(defn color-str [[hue sat] lum alpha]
  (if alpha
    (format "hsla(%d, %d%%, %d%%, %s)"
            (Math/round (* 360.0 hue))
            (Math/round (* 100.0 sat))
            (Math/round (* 100.0 lum))
            (lossy-float-str alpha))
    (format "hsl(%d, %d%%, %d%%)"
            (Math/round (* 360.0 hue))
            (Math/round (* 100.0 sat))
            (Math/round (* 100.0 lum)))))

(defn evaluate [defs sym]
  (cond
   (list? sym)
   (let [[f & body] sym]
     (condp = f
       ;; Obsolete
       'get (get defs (first body))

       ;; TODO: use single quotes instead of double quotes.
       'str (pr-str
             (apply str (for [x body]
                          (evaluate defs x))))

       'call (let [[head & args] body]
               (str (evaluate defs head)
                    \( (clojure.string/join "," (map #(evaluate defs %)
                                                     args)) \)))
       'sprite (when (= (count body) 2)
                 (let [[resource cell-width cell-height m] (get defs (first body))
                       resource (evaluate defs resource)
                       cell-width (evaluate defs cell-width)
                       cell-height (evaluate defs cell-height)
                       spr-ind (get m (second body))]
                   (when spr-ind
                     (str "url(" (pr-str resource) ") "
                          (mul cell-width (* -1 (first spr-ind))) " "
                          (mul cell-height (* -1 (second spr-ind))) " no-repeat"))))

       "NOT IMPLEMENTED YET"))

   (symbol? sym)
   (let [v (name sym)]
     (or
      (when-let [[_ percent]
                 (re-matches #"%([-.0-9]+)" v)]
        (str percent "%"))

      (when-let [[_ unit quantity]
                 (re-matches #"([a-z]+)\*([-+.0-9]+)" v)]
        ;; should be read-number but we don't have that apparently.
        (NumberWithUnit. (read-string quantity) unit)
        ;; (str quantity unit)
        )

      (when-let [[_ hue-sat lum _ alpha]
                 (re-matches #"([a-zA-Z-]+)(\.?[0-9]+)(:([.0-9]+))?" v)]
        (color-str (or (get defs (symbol hue-sat))
                       (get known-colors-hue-sat hue-sat))
                   (Double/valueOf lum)
                   (when alpha (Double/valueOf alpha))))

      (when-let [sym-value (get defs sym)]
        (pr-str sym-value))

      v))

   :else
   (str sym)))

(defn evaluate-attr-value [defs attr-value]
  (cond
   (vector? attr-value)
   (string/join " "
                        (for [v attr-value]
                          (evaluate defs v)))
   :else
   (evaluate defs attr-value)))

(def needs-vendors #{:border-radius :column-width :column-gap})

(defn process-attrs [defs attributes]
  (mapcat :attr-set
          (sort-by :key
                   (for [[key raw-v] attributes]
                     {:key key
                      :attr-set
                      (let [v (evaluate-attr-value defs raw-v)
                            k (name key)]
                        (if (needs-vendors key)
                          [[(str "-moz-" k) v]
                           [(str "-webkit-" k) v]
                           [(str "-o-" k) v]
                           [(str k) v]]
                          [[(name k) v]]))}))))

(defn print-tree [tree]
  (let [body (rulize tree)
        defs (make-context (body :defs))]

    (when (defs :ns)
      (println (format "/* namespace: %s */" (defs :ns))))

    (doseq [row (sort-by #(first (get % :selectors))
                         (for [[selectors attributes] (body :rules)]
                           {:selectors selectors
                            :attributes (process-attrs defs attributes)}))]
      (println)
      (println (string/join
                ",\n"
                (for [selector (row :selectors)]
                  ;; Prepend the namespace to the first piece of the selector
                  ;; if that first piece is a class or id selector.
                  (if-let [css-ns (get defs :ns)]
                    (if (re-find #"^[.#]" selector)
                      (str (.substring selector 0 1)
                           css-ns "-"
                           (.substring selector 1))
                      selector)
                    selector)))
               "{")
      (doseq [[k v] (row :attributes)]
        (println (format "  %s: %s;" k v)))
      (println "}"))))


;; A very dumb parser quickly put together for experimenting.
(def whitespace-re #"[\s\n\r]")
(def special-re  #"[(){}\[\]\",';/:]")
(defn tokenize [s]
  (loop [s (str s) ;; TODO: use a charbuffer instead of a string.
         i 0
         state :low
         l []]
    (cond
     (empty? s) l

     (>= i (count s)) (conj l s)  ;; flush the rest into s.

     :else
     (let [c (str (.charAt s i))]
       (cond
        (re-matches whitespace-re c)
        (if (= :high state)
          (recur
           (.substring s (inc i)) 0 :low (conj l (.substring s 0 i)))
          (recur
           (.substring s 1) 0 :low l))

        (re-matches special-re c)
        (if (= :high state)
          (recur
           (.substring s (inc i)) 0 :low (conj l (.substring s 0 i) c))
          (recur
           (.substring s 1) 0 :low (conj l c)))

        :else
        (recur s (inc i) :high l))))))

(defn css-read-phase2 [coll]
  ;; coll is the tokenized string.
  (loop [coll coll
         stack ()
         hand [:file]]
    (if (empty? coll)
      (if (empty? stack)
        hand
        nil)

      (let [[x & coll] coll]
        (cond
         (= x "(")
         (recur coll (cons hand stack) [:list])

         (= x "[")
         (recur coll (cons hand stack) [:vector])

         (= x "{")
         (recur coll (cons hand stack) [:mapping])

         (or (= x "]") (= x "}") (= x ")"))
         (let [[top & stack] stack]
           (recur coll stack (conj top hand)))

         :else
         (recur coll stack (conj hand x)))))))

;; Read messy "tree" and produce a better tree.
;; format:
;; header1 , header2 , ... { key : values ; ... }
(defn css-read-phase3 [tree])
