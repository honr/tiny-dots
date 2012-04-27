(ns rose.gplot
  (require [clojure.string :as string])
  (:require [rose.file]
            [rose.utils]
            [rose.clu :only [sh]]))

;; from clojure.contrib.combinatorics
(defn cartesian-product
  "All the ways to take one item from each sequence"
  [& seqs]
  (let [v-original-seqs (vec seqs)
	step
	(fn step [v-seqs]
	  (let [increment
		(fn [v-seqs]
		  (loop [i (dec (count v-seqs)), v-seqs v-seqs]
		    (if (= i -1) nil
			(if-let [rst (next (v-seqs i))]
			  (assoc v-seqs i rst)
			  (recur (dec i) (assoc v-seqs i (v-original-seqs i)))))))]
	    (when v-seqs
              (cons (map first v-seqs)
                    (lazy-seq (step (increment v-seqs)))))))]
    (when (every? seq seqs)
      (lazy-seq (step v-original-seqs)))))

(defn spit-matrix [filename coll & {sep :sep}]
  (let [sep (or sep " ")]
    (spit 
     filename
     (string/join "\n"
                  (apply map (fn [& row] 
                               (string/join sep (map #(if %
                                                        (.doubleValue %)
                                                        Double/NaN)
                                                     row)))
                         coll)))))

;; (spit-matrix "data" [[1 2 3] [4 5 6]])
;; (spit-matrix "data2" [[1 2 3] [4 5 6]] :sep ",")

(def ^{:private true} font
  (atom {:plain (cond rose.utils/os-linux?
                      (rose.file/path :home ".fonts" "times.ttf")

                      rose.utils/os-macosx?
                      "/Library/Fonts/Times New Roman.ttf")
         :ps "Times"}))

(defn- process-prm-out [file-base term settings]
  ({"png"
    [(format "set term png enhanced font \"%s\"" (or (:font settings) (@font :plain)))
     (format "set output \"%s.png\"" file-base)]

    "eps"
    [(format "set term postscript eps 22 enhanced font \"%s\"" (or (:font settings) (@font :ps)))
     (format "set output \"%s.eps\"" file-base)]

    "pdf"
    [(format "set term postscript eps 22 enhanced font \"%s\"" (or (:font settings) (@font :ps)))
     (format "set output \"%s.eps\"" file-base)]

    "svg"
    ["set term svg"
     (format "set output \"%s.svg\"" file-base)]
    
    "tex" ;; actually pstricks
    ["set term pstricks"
     (format "set output \"%s.tex\"" file-base)]

    ;; todo: add tex
    
    nil
    [(cond
       rose.utils/os-linux? "set term wxt enhanced"
       rose.utils/os-macosx? "set term x11")]} term))

(def line-pallettes
  {:pallette {:two  (into [] (cartesian-product (range 1 10) [6 2]))
              :one  (into [] (map vector (range 1 10) (range 1 10)))
              :one2 (into [] (map vector
                                  [1 3 2 4 5 6 7 8 9  0] ; color
                                  [6 2 7 4 5 1 8 3 9 10 11 12 13 14 15]))}
   :color [:silver :red :green :blue :magenta :cyan :yellow :black :orange]
   :point [:unknown :plus :cross :star8 :box :box-full :circle :circle-full
           :triangle-up :triangle-up-full :triangle-down :triangle-down-full
           :diamond :diamond-full]})

(defn plt-color [plt]
  (map (fn [[c p]]
         [(get (get line-pallettes :color)
               (mod c (count (get line-pallettes :color))))
           (get (get line-pallettes :point)
                (mod p (count (get line-pallettes :point))))])
       (get-in line-pallettes [:pallette plt])))

(defn- process-prm-plt [plt & [lw ps]]
  (let [pallette (get-in line-pallettes [:pallette plt])]
    (map #(format
    	   "set style line %d lc %d pt %d lw %.1f ps %.1f"
    	   (inc %)
    	   (first  (pallette %)) ; line color
    	   (second (pallette %)) ; point type
    	   (.doubleValue (or lw 1))    ; line width
    	   (.doubleValue (or ps 1.5))) ; point size
    	 (range (count pallette)))))

(defn- process-prm-ls [filename t]
  [(if (coll? t)
     (str "plot" (string/join ",\\\n    " (map #(format " '%s' using 1:%d with linespoints ls %d title '%s'" filename (+ 2 %1) (+ 1 %1) %2) (range (count t)) t)))
     (str "plot" (string/join ",\\\n    " (map #(format " '%s' using 1:%d with linespoints" filename (+ 2 %)) (range t)))))])

(defn- process-prm-set [arg]
  (cond
    (vector? arg)
    (let [[command & body] arg]
      ({:log (string/join " " (into ["set logscale"] body))
        :key (str "set key " (string/join " " (map {\n "top" \t "top"
                                                    \w "left" \l "left" 
                                                    \e "right" \r "right"
                                                    \s "bottom" \b "bottom"
                                                    \c "center"
                                                    \o "outside" 
                                                    \i "inside"} 
                                                   (first body))))
        :tx (format "set xlabel '%s'" (first body))
        :ty (format "set ylabel '%s'" (first body))
        :tt (format "set title '%s'" (first body))
        :format (format "set format %s '%s'" (first body) (second body))}
       command))

    (string? arg)
    (str "set " arg)))

(defn- coll-maybe [x]
  (if (coll? x)
    x
    (list x)))

(defn ploti [commands & cols]
  (let [[file-base out-type out-settings] (let [[file-maybe & [out-cmd-rest]] (coll-maybe (:out commands))
                                                [_ b e] (re-matches #"(.*)\.([^.]*)" file-maybe)]
                                            [(or b "/tmp/" *username* "-gnuplot-dummy") e out-cmd-rest])
	file-data (str file-base ".data")
	file-gnuplot (str file-base ".gnuplot")]
    (spit-matrix file-data cols)
    (spit file-gnuplot
          (string/join "\n" 
                       (filter identity 
                               (concat
                                (process-prm-out file-base out-type out-settings)
                                (apply process-prm-plt (coll-maybe (:plt commands)))
                                (map process-prm-set (:set commands))
                                (process-prm-ls file-data (or (:ls commands) (dec (count cols))))
                                (when (= :replot (:console commands)) ["set term wxt enhanced" "replot"])
                                (when-not (or (:console commands) (:out commands)) ["pause mouse close"])))))
    (cond
      (not (:console commands))
      (:out (rose.clu/sh "gnuplot" file-gnuplot))
      
      rose.utils/os-linux?
      (:out (rose.clu/sh "gnome-terminal" "--geometry" "80x8" "-e" (format "gnuplot \"%s\" -" file-gnuplot)))

      rose.utils/os-macosx?
      (:out (rose.clu/sh "xterm" "-geometry" "80x8" "-e" (format "gnuplot \"%s\" -" file-gnuplot))))
    
    (when (= out-type "pdf")
      (:out (rose.clu/sh "epstopdf" "--outfile" (str file-base ".pdf") (str file-base ".eps"))))))

;; TODO:
;; 1. The model of xs, [y1s, y2s, ..., yns], [l1, l2, ..., ln] is not sufficient
;;    We need
;;      - [x1s y1s], [x2s y2s], ..., [xns, yns], [l1, l2, ..., ln]
;;      - [x1s, x2s, ..., xns], ys, [l1, l2, ..., ln]
;;    suggestion:
;;    (plot [[x1 y1 l1] [x2 y2 l2] [x3 y3 l3]] opts)
;;    (plot x1 [[y1 l1] [y2 l2] [y3 l3]] opts)
;;
;; 2. Output feature: replot.
;; 3. Do not write down the line styles that are not used.

(defn plot []
  (let [[file-base out-type out-settings] (let [[file-maybe & [out-cmd-rest]] (coll-maybe (:out commands))
                                                [_ b e] (re-matches #"(.*)\.([^.]*)" file-maybe)]
                                            [(or b "/tmp/" *username* "-gnuplot-dummy") e out-cmd-rest])
	file-data (str file-base ".data")
	file-gnuplot (str file-base ".gnuplot")]
    (spit-matrix file-data cols)
    (spit file-gnuplot
          (string/join "\n" 
                       (filter identity 
                               (concat
                                (process-prm-out file-base out-type out-settings)
                                (apply process-prm-plt (coll-maybe (:plt commands)))
                                (map process-prm-set (:set commands))
                                (process-prm-ls file-data (or (:ls commands) (dec (count cols))))
                                (when (= :replot (:console commands)) ["set term wxt enhanced" "replot"])
                                (when-not (or (:console commands) (:out commands)) ["pause mouse close"])))))
    (cond
      (not (:console commands))
      (:out (rose.clu/sh "gnuplot" file-gnuplot))
      
      rose.utils/os-linux?
      (:out (rose.clu/sh "gnome-terminal" "--geometry" "80x8" "-e" (format "gnuplot \"%s\" -" file-gnuplot)))

      rose.utils/os-macosx?
      (:out (rose.clu/sh "xterm" "-geometry" "80x8" "-e" (format "gnuplot \"%s\" -" file-gnuplot))))
    
    (when (= out-type "pdf")
      (:out (rose.clu/sh "epstopdf" "--outfile" (str file-base ".pdf") (str file-base ".eps"))))))

(comment
  (ploti {:out "/tmp/a" ;; or ["a.png"] or "a.png" or ["a.png" {:font "Times"}]
          :plt nil ;; :one :one2 :two
          :set [[:tx "alpha"]
                ;; [:logscale "y"]
                "logscale y"
                [:key "tl"]]
          :ls ["before" "after"]
          :console true}
         (range 1 64)
         (map #(Math/sin (* 0.03 %)) (range 1 64))
         (map #(Math/sin (* 0.06 %)) (range 1 64)))
  
  )

;; plot 'data' with lines
(defn rggobi [& cols]
  (spit-matrix "data" cols :sep ",")
  (:out (rose.clu/sh "ggobi" "-d" "csv" "data")))

;; set logscale

;;   (apply gploti
;; 	 {:set ["logscale"]
;; 	  :tx tx
;; 	  :ty "prob. failure"
;; 	  :ls (count res)
;; 	  }
;; 	 x res)

;; (gploti {:console true :set ["term x11"] :ls ["aa"]} (range 10) (range 10) )

;; set format xy "$10^{%T}$"
