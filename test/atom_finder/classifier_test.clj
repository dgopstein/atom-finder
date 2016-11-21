(ns atom-finder.classifier-test
  (:require [clojure.test :refer :all]
            [atom-finder.util :refer :all]
            [atom-finder.classifier :refer :all]
            ))


(deftest test-short-circuitable?
  (testing "Nodes that can short-circuit"
    (let [cases 
          [["1 && 2"   true]
           ["1 || 2"   true]
           ["0 && 1"   true]
           ["0 || 1"   true]
           ["1 && f()" true]
           ["f() || 2" true]
           ["1 && 2;"  false]
           ["2"        false]
           ["f()"      false]
           ["1 + 2"    false]]]

          (doseq [[code sc?] cases]
            (is (short-circuitable? (parse-expr code)) sc?))
    )))
