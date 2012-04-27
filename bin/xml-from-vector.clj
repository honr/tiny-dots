#!/usr/bin/env clove
;; | clojure

(ns bin.xml-from-vector
  (:require [rose.xml]))

(print (rose.xml/s<-v (read-string (slurp *in*))))
