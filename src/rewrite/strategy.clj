(ns rewrite.strategy
  (:require [rewrite.rewrite :refer :all]
            [rewrite.term :refer :all]))

;; Question 3.1
(defn success [t] t)

(defn fail [t] nil)

;; Question 3.2
(defn rule [[lhs rhs]]
  (fn [t]
    (if-let [s (match lhs t)]
      (subst rhs s)
      nil)))

;; Question 3.3
(defn then [s1 s2]
  (fn [t]
    (if-let [t1 (s1 t)]
      (s2 t1)
      nil)))

;; Question 3.4
(defn orelse [s1 s2]
  (fn [t]
    (if-let [t (s1 t)]
      t
      (s2 t))))

;; Question 3.5
(defn sub [s]
  (fn [t]
    (if (sequentiel? t)
      (let [t1 (map (orelse s success) t)]
        (if (= t1 t) nil t1))
      nil)))

;; Question 3.6
(defn rec [s]
  (fn [t]
    ((orelse
      (orelse
       (then (sub (rec s)) s)
       (sub (rec s)))
      s) t)))
