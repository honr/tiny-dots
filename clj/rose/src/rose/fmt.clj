(ns rose.fmt ;; Text formatting utilities.
  (:require [clojure.string :as string]))

(defn format-num-units [& nums-singular-plural]
  (.substring
   (apply str
          (for [[^Number n ^String unit-singular ^String unit-plural]
                nums-singular-plural]
            (when (pos? n)
              (str ", " n " " (if (= n 1)
                                unit-singular
                                unit-plural)))))
   2))