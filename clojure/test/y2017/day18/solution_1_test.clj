(ns y2017.day18.solution-1-test
  (:require [y2017.day18.solution-1 :refer [part-1]]))

(require '[clojure.test :refer [deftest is]])

(deftest test-part-1-test
  (is (= 4 (part-1 "resources/y2017/day18/test-1.txt"))))

(deftest test-part-1-real
  (is (= 1187 (part-1 "resources/y2017/day18/real.txt"))))