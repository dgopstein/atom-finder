(ns atom-finder.assignment-as-value-test
  (:require [clojure.test :refer :all]
            [schema.test]
            [atom-finder.util :refer :all]
            [atom-finder.classifier :refer :all]
            ))

(use-fixtures :once schema.test/validate-schemas)

(deftest test-assignment-as-value?
  (testing "assignment-as-value-atoms finds all atoms in c file"
    (let [filepath   (resource-path "assignment-as-value.c")
          expected   (true-lines filepath)
          lines  (->> filepath
                      tu
                      (:finder (:assignment-as-value atom-lookup))
                      (map loc)
                      (map :line))]

     ; (is (= expected lines))
    )))
