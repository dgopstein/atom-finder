(ns atom-finder.range-set-test
  (:require [clojure.test :refer :all]
            [schema.test]
            [atom-finder.util :refer :all]
            [atom-finder.classifier :refer :all]
            [clojure.pprint :refer [pprint]]
            ))

(use-fixtures :once schema.test/validate-schemas)

(deftest test-range-set
  (testing "canonical range set"
    (let [cases [
                 [[1 2 3 5]  "[[1..4), [5..6)]"]
                 ]]

      (doseq [[elems expected] cases]
        (testing (str elems " -> " expected)
          (is (= expected (str (range-set-canonical elems)))))))))
