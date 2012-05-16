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

(defn- coll-maybe [x]
  (if (coll? x)
    x
    (list x)))

(defn- spit-rows [filename coll]
  (with-open [w (java.io.PrintWriter. filename)]
    (doseq [row coll]
      (when row
        (.println w row)))))

(defn- sh-run-rows [coll]
  (doseq [row coll]
    (when row
      (println (:out (apply rose.clu/sh row))))))

(defn- spit-matrix [filename coll & {sep :sep}]
  ;; (spit-matrix "data" [[1 2 3] [4 5 6]])
  ;; (spit-matrix "data2" [[1 2 3] [4 5 6]] :sep ",")
  (let [sep (or sep " ")]
    (spit-rows
     filename
     (apply map (fn [& row] 
                  (string/join sep (map #(if %
                                           (.doubleValue %)
                                           Double/NaN)
                                        row)))
            coll))))

(def ^{:private true :dynamic true} *font*
  {:plain (cond rose.utils/os-linux?
                (rose.file/path :home ".fonts" "times.ttf")

                rose.utils/os-macosx?
                "/Library/Fonts/Times New Roman.ttf")
   :x "Times"
   :ps "Times"})

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

(defn- process-prm-plt [num-used-lines plt lw ps]
  (let [pallette (if (coll? plt)
                   plt
                   (get-in line-pallettes [:pallette plt]))]
    (map #(format
    	   "set style line %d lc %d pt %d lw %.1f ps %.1f"
    	   (inc %)
    	   (first  (pallette %)) ; line color
    	   (second (pallette %)) ; point type
    	   (.doubleValue (or lw 1))    ; line width
    	   (.doubleValue (or ps 1.5))) ; point size
    	 (range (min (count pallette)
                     num-used-lines)))))

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

;; set format xy "$10^{%T}$"

(defn process-prm-out [out opts]
  (let [[file-base term] (let [[_ b e] (re-matches #"(.*)\.([^.]*)" out)]
                           [(or b "/tmp/" *username* "-gnuplot-dummy") e])
        file-data (str file-base ".data")
        file-gnuplot (str file-base ".gnuplot")]
    {:file-data file-data
     :file-gnuplot file-gnuplot
     :out-init
     ({"png"
       [(format "set term png enhanced font \"%s\" size %d,%d"
                (or (opts :font) (*font* :plain))
                (or (opts :width) 600)
                (or (opts :height) 400))
        (format "set output \"%s.png\"" file-base)]

       "eps"
       [(format "set term postscript eps 22 enhanced font \"%s\" size %d,%d"
                (or (opts :font) (*font* :ps))
                (or (opts :width) 600)
                (or (opts :height) 400))
        (format "set output \"%s.eps\"" file-base)]

       "pdf"
       [(format "set term postscript eps 22 enhanced font \"%s\" size %d,%d"
                (or (opts :font) (*font* :ps))
                (or (opts :width) 600)
                (or (opts :height) 400))
        (format "set output \"%s.eps\"" file-base)]

       "svg"
       [(format "set term svg size %d,%d"
                (or (opts :width) 600)
                (or (opts :height) 400))
        (format "set output \"%s.svg\"" file-base)]
       
       "pstricks"
       ["set term pstricks"
        (format "set output \"%s.tex\"" file-base)]

       ;; TODO: Add tex output.

       "canvas"
       [(format "set terminal canvas size %d,%d fsize %d lw 1 jsdir '%s'"
                (or (opts :width) 600)
                (or (opts :height) 400)
                (or (opts :fsize) 12)
                (or (opts :jsdir) "/usr/share/gnuplot/gnuplot/4.4/js"))
        (format "set output '%s.html'" file-base)]

       
       nil
       [(cond
         rose.utils/os-linux? (format "set term wxt enhanced persist font '%s'"
                                      (or (opts :font) (*font* :x)))
         rose.utils/os-macosx? "set term x11")]} term)

     :out-end (concat
               (when (= :replot (opts :console)) ["set term wxt enhanced" "replot"])
               (when (and (not (opts :console)) (not out)) ["pause mouse close"]))
     
     ;; Vector of commands lines to run, each command line is itself a vector.
     :sh
     [(cond
       (not (opts :console))
       ["gnuplot" file-gnuplot]
       
       rose.utils/os-linux?
       ["gnome-terminal" "--geometry" "80x8" "-e" (format "gnuplot \"%s\" -" file-gnuplot)]

       rose.utils/os-macosx?
       ["xterm" "-geometry" "80x8" "-e" (format "gnuplot \"%s\" -" file-gnuplot)])
      
      (when (= term "pdf")
        ["epstopdf" "--outfile" (str file-base ".pdf") (str file-base ".eps")])]}))

(defn process-prm-plot [filename fn-plot-line with-what legends raw-plot]
        [(str "plot \\\n"
              (string/join
               ", \\\n"
               (concat
                (map (fn [i legend]
                       (format "  '%s' using %s with %s ls %d title '%s'"
                               filename
                               (fn-plot-line i)
                               with-what
                               (inc i)
                               legend))
                     (range (count legends))
                     legends)
                raw-plot)))])

(defn plot-generic [opts
                    matrix
                    legends
                    fn-plot-line]
  (let [out (process-prm-out (:out opts) opts)]
    (spit-matrix (out :file-data) matrix)
    (spit-rows
     (out :file-gnuplot)
     (concat
      (out :out-init)
      (process-prm-plt (count legends) (:plt opts) (:lw opts) (:ps opts))
      (map process-prm-set (:set opts))
      (process-prm-plot (out :file-data)
                        fn-plot-line
                        (or (opts :with) "linespoints")
                        legends
                        (opts :raw-plot))
      (out :out-end)))
    (sh-run-rows (out :sh))))

;; TODO:
;; 1. Output feature: replot. Should we merge console and out?
;; 2. Do not write down the line styles that are not used.

(defn plot
  ([x-y-ls opts] ;; (plot [[x1s y1s l1] [x2s y2s l2] ... [xns yns ln]] opts)
     (plot-generic opts
                   (mapcat (fn [[x y legend]] [x y]) x-y-ls)
                   (map (fn [[x y legend]] legend) x-y-ls)
                   (fn [i] (format "%d:%d" (+ 1 (* 2 i)) (+ 2 (* 2 i))))))
  ([xs y-ls opts] ;; (plot xs [[y1s l1] [y2s l2] ... [yns ln]] opts)
     (plot-generic opts
                   (cons xs (map first y-ls))
                   (map second y-ls)
                   (fn [i] (format "1:%d" (+ 2 i))))))


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
