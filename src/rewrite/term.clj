(ns rewrite.term
  (:require [clojure.set]))

(defn variable? [t]
  (and (symbol? t)
       (= (first (name t)) \?)))

(defn sequentiel? [t]
  (sequential? t))

(defn constante? [t]
  (and (not (nil? t))
       (not (variable? t))
       (not (sequentiel? t))))

(defn terme? [t]
  (or (constante? t)
      (variable? t)
      (sequentiel? t)))

;; Question 1.1
(defn variables [t]
  (cond
    (variable? t) #{t}
    (constante? t) #{}
    :else (reduce clojure.set/union #{} (map variables t))))

;; Question 1.2
(defn pattern? [t]
  (not (empty? (variables t))))

(defn ground? [t]
  (empty? (variables t)))
