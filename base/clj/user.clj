(ns user
  (:require [clojure.repl]
            [clojure.string :as string]))

(defn prcoll [coll]
  (doseq [x coll]
    (println x)))

(defn doc-class [cls]
  (println "Class:" (.getName cls))
  (println "Constructors:")
  (prcoll
   (map second
        (sort-by #(.getName (first %))
                 (for [mthd (.getConstructors cls)]
                   [mthd (format "   %s%s %s %s : \033[31m%s\033[0m"
                                 ;; (if (.isAccessible mthd) "A" "-")
                                 (if (.isSynthetic mthd) "S" "-")
                                 (if (.isVarArgs mthd) "V" "-")
                                 (.getName mthd)
                                 (vec (map #(format "\033[36m%s\033[0m" %)
                                           (.getParameterTypes mthd)))
                                 (vec (map #(format "\033[36m%s\033[0m" %)
                                           (.getTypeParameters mthd))))]))))
  (println "Methods:")
  (prcoll
   (map second
        (sort-by #(.getName (first %))
                 (for [mthd (.getMethods cls)]
                   [mthd (format "  %s%s%s %s %s : \033[31m%s\033[0m"
                                 ;; (if (.isAccessible mthd) "A" "-")
                                 (if (.isBridge mthd) "B" "-")
                                 (if (.isSynthetic mthd) "S" "-")
                                 (if (.isVarArgs mthd) "V" "-")
                                 (.getName mthd)
                                 (vec (map #(format "\033[36m%s\033[0m" %)
                                           (.getParameterTypes mthd)))
                                 (.getReturnType mthd))]))))
  (println "Fields:")
  (prcoll
   (map second
        (sort-by #(.getName (first %))
                 (for [mthd (.getFields cls)]
                   [mthd (format "   %s%s %s : \033[31m%s\033[0m"
                                 ;; (if (.isAccessible mthd) "A" "-")
                                 (if (.isEnumConstant mthd) "E" "-")
                                 (if (.isSynthetic mthd) "S" "-")
                                 (.getName mthd)
                                 (.getType mthd))])))))
