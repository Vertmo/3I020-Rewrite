(ns rewrite.rewrite
  (:require [clojure.set]
            [rewrite.term :refer :all]))

(defn regle-ok? [[lhs rhs]]
  (clojure.set/subset? (variables rhs) (variables lhs)))

;; Question 2.1

(declare match)

(defn match-seq [p t]
  (if (not (= (count p) (count t)))
    nil
    (loop [p p, t t, m {}]
      (if (seq p)
        (if-let [nm (match (first p) (first t))]
          (if (reduce (fn [a k]
                        (or a (and (contains? m k)
                                  (not (= (get m k) (get nm k))))))
                      false
                      (keys nm))
            nil
            (recur (rest p) (rest t) (conj m nm)))
          nil)
        m))))

(defn match [p t]
  (cond
    (ground? p) (if (= p t) {} nil)
    (variable? p) {p t}
    (sequentiel? t) (match-seq p t)))

;; Question 2.2
(defn subst [t s]
  (cond
    (ground? t) t
    (variable? t) (get s t)
    :else (map #(subst % s) t)))

;; Question 2.3
(defmacro defrule [nom lhs fleche rhs]
  (if (= fleche '=>)
    `(def ~nom '[~lhs ~rhs])
    `()))
