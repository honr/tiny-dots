#!/usr/bin/env clove
;; | clojure

(ns bin.xml-to-vector
  (:require [rose.xml]
            [clojure.pprint]))

(clojure.pprint/pprint (rose.xml/v<-s (slurp *in*)) *out*)
