(ns rewrite.rewrite-test
  (:require  [clojure.test :refer :all]
             [rewrite.rewrite :refer :all]))

(deftest regle-ok?-test
  (is (= (regle-ok? '[(?X + 0) ?X]) true))
  (is (= (regle-ok? '[(?X + ?Y) (?Z - ?X)])) false)
  (is (= (regle-ok? '[(2 + 2) 4])) true))

(deftest match-test
  "Question 2.1"
  (is (= (match '(?X + 0) '((3 * 2) + 0))
         '{?X (3 * 2)}))
  (is (= (match '?X 42)
         '{?X 42}))
  (is (= (match '(?X + ?Y) '((3 * 2) + 42))
         '{?X (3 * 2) ?Y 42}))
  (is (= (match 42 42))
      {})
  (is (= (match 34 42))
      nil)
  (is (= (match '(?X + ?Y) '(2 - 2))
         nil)))

(deftest subst-test
  "Question 2.2"
  (is (= (subst '?X '{?X (3 * 2)})
         '(3 * 2)))
  (is (= (subst '(?Y * ?X + 2) '{?X 2 ?Y (3 - 2)})
         '((3 - 2) * 2 + 2)))
  (is (= (subst 42 {})
         42)))

(deftest defrule-test
  "Question 2.3"
  (is (= (macroexpand
          '(defrule plus-dist-fois
             (?X + (?Y * ?Z)) => ((?X * ?Y) + (?X * ?Z)))))
      '(def plus-dist-fois
         '[(?X + (?Y * ?Z)) ((?X * ?Y + (?X * ?Z)))])))
