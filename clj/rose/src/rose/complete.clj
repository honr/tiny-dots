(ns rose.complete)

(defprotocol compable
  (complete [this] "complete this"))

;; (defmacro deftype [name & args]
;;   (str *ns* name)
;;   ;; (clojure.core/deftype ~name ~@args)
;;   )
