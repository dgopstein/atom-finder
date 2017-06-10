(ns atom-finder.util-test
  (:require [clojure.test :refer :all]
            [atom-finder.util :refer :all]
            ))

(deftest result-flattening
  (testing "merge-down"
    (is (= {:a-b 1, :a-c-d 2, :d 4} (merge-down {:a {:b 1 :c {:d 2 }} :d 4})))))

