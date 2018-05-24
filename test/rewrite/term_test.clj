(ns rewrite.term-test
  (:require  [clojure.test :refer :all]
             [rewrite.term :refer :all]))

(deftest terme?-test
  (is (= (terme? '(:rouge :vert ?X :rouge))) true)
  (is (= (terme? '(2 + (3 * ?X) - 5))) true))

(deftest variables-test
  "Question 1.1"
  (is (= (variables '(2 + (3 * ?X) - ?X * ?Y))
         #{'?X '?Y}))
  (is (= (variables '(:rouge :vert :rouge :vert))
         #{})))
