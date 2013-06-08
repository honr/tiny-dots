(ns rose.ss-css
  (:require [clojure.string]))

;; TODO: allow inserting comments and section headers.

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
            (cond
             (= f 'def)
             [(first row-body) (second row-body)]

             (= f 'ns)
             [:ns (first row-body)]
              ;; We do nothing for the rest of them yet!
              )))))

(def known-colors-hue-sat
  {"red" [0.1 0.2]})

(defn lossy-float-str [x]
  (let [s (format "%.2f" x)]
    (if (< -1 x 1)
      (.substring s 1))))

(defn color-str [[hue sat] lum alpha]
  (if alpha
    (format "hsla(%d, %d%%, %d%%, %s)"
            (Math/round (* 360 hue))
            (Math/round (* 100 sat))
            (Math/round (* 100 lum))
            (lossy-float-str alpha))
    (format "hsl(%d, %d%%, %d%%)"
            (Math/round (* 360 hue))
            (Math/round (* 100 sat))
            (Math/round (* 100 lum)))))

(defn process-attr-value [defs v]
  (let [process #(process-attr-value defs v)]
    (cond
     (vector? v)
     (clojure.string/join
      " "
      (for [item v]
        (process-attr-value defs item)))

     (list? v)
     (let [[f & body] v]
       (condp = f
           'get (get defs (first body))

           ;; TODO: use single quotes instead of double quotes.
           'str (pr-str
                 (apply str (for [x body]
                              (process-attr-value defs x))))

           "NOT IMPLEMENTED YET"))

     (symbol? v)
     (let [v (name v)]
       (or
        (when-let [[_ percent]
                   (re-matches #"%([-.0-9]+)" v)]
          (str percent "%"))

        (when-let [[_ unit quantity]
                   (re-matches #"([a-z]+)\*([-+.0-9]+)" v)]
          (str quantity unit))

        (when-let [[_ hue-sat lum _ alpha]
                   (re-matches #"([a-z]+)(\.?[0-9]+)(:([.0-9]+))?" v)]
          (color-str (or (get defs (symbol hue-sat))
                         (get known-colors-hue-sat hue-sat))
                     (Double/valueOf lum)
                     (when alpha (Double/valueOf alpha))))
        v))

     :else
     (str v))))

(def needs-vendors #{:border-radius})

(defn process-attrs [defs attributes]
  (mapcat :attr-set
          (sort-by :key
                   (for [[key raw-v] attributes]
                     {:key key
                      :attr-set
                      (let [v (process-attr-value defs raw-v)
                            k (name key)]
                        (if (needs-vendors key)
                          [[(str "-moz-" k) v]
                           [(str "-webkit-" k) v]
                           [(str k) v]]
                          [[(name k) v]]))}))))

(defn print-tree [tree]
  (let [body (rulize tree)
        defs (make-context (body :defs))]

    (println (format "/* namespace: %s */" (or (defs :ns) "unknown!")))

    (doseq [row (sort-by :selectors
                         (for [[selectors attributes] (body :rules)]
                           {:selectors selectors
                            :attributes (process-attrs defs attributes)}))]
      (println)
      (println (clojure.string/join
                ",\n"
                (for [selector (row :selectors)]
                  ;; Prepend the namespace to the first piece of the selector if
                  ;; that first piece is a class or id selector.
                  (if (re-find #"^[.#]" selector)
                    (str (.substring selector 0 1)
                         (get defs :ns) "-"
                         (.substring selector 1))
                    selector)))
               "{")
      (doseq [[k v] (row :attributes)]
        (println (format "  %s: %s;" k v)))
      (println "}"))))
