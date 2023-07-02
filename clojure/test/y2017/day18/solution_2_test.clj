(ns y2017.day18.solution-2-test
  (:require [y2017.day18.solution-2 :refer [part-2]]))

(require '[clojure.test :refer [deftest is]])

(deftest test-part-2-test-2
  (is (= 3 (part-2 "resources/y2017/day18/test-2.txt"))))

(deftest test-part-2-test-3
  (is (= 1 (part-2 "resources/y2017/day18/test-3.txt"))))

(deftest test-part-2-real
  (is (= 5969 (part-2 "resources/y2017/day18/real.txt"))))