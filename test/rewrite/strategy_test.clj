(ns rewrite.strategy-test
  (:require  [clojure.test :refer :all]
             [rewrite.rewrite :refer :all]
             [rewrite.strategy :refer :all]))

(defrule plus-dist-fois
  (?X + (?Y * ?Z)) => ((?X * ?Y) + (?X * ?Z)))

(def strat1 (rule plus-dist-fois))

(deftest rule-test
  "Question 3.2"
  (is (= (strat1 '(2 + (3 * 4)))
         '((2 * 3) + (2 * 4))))
  (is (= (strat1 '(2 * (3 + 4)))
         nil)))

(defrule plus-commute (?X + ?Y) => (?Y + ?X))
(defrule plus-zero (?X + 0) => ?X)

(deftest commute-and-zero-test
  (is (= ((rule plus-zero) '(0 + 42))
         nil))
  (is (= ((rule plus-commute) '(0 + 42))
         '(42 + 0)))
  (is (= ((rule plus-zero) '(42 + 0))
         42)))

(def strat2 (then (rule plus-commute) (rule plus-zero)))

(deftest then-test
  "Question 3.3"
  (is (= (strat2 '(0 + 42))
         42)))

(def strat3 (orelse
             (rule plus-zero)
             (then (rule plus-commute) (rule plus-zero))))

(deftest orelse-test
  "Question 3.4"
  (is (= (strat3 '(42 + 0))
         42))
  (is (= (strat3 '(0 + 42))
         42))
  (is (= (strat3 '(42 * 1))
         nil)))

(def strat4 (sub (rule plus-zero)))

(deftest sub-test
  "Question 3.5"
  (is (= (strat4 '((3 * 2) (42 + 0) 40))
         '((3 * 2) 42 40)))
  (is (= (strat4 '((3 * 2) (0 + 42) 42))
         nil))
  (is (= (strat4 42)
         nil)))

(def strat5 (rec (rule plus-zero)))

(deftest rec-test
  "Question 3.6"
  (is (= (strat5 '((3 * (42 + 0)) + ((42 + 0) + 0) - 40))
         '((3 * 42) + 42 - 40)))
  (is (= (strat5 '((3 * 2) + (1 * 42) - 40))
         nil)))
