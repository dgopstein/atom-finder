(ns atom-finder.classifier-util-test
  (:require [clojure.test :refer :all]
            [schema.test]
            [atom-finder.util :refer :all]
            [atom-finder.test-util :refer :all]
            [atom-finder.classifier-util :refer :all]
            [atom-finder.classifier :refer :all]
            ))

(use-fixtures :once schema.test/validate-schemas)

(deftest test-numeric-literal
  (testing "Is this expression a numeric literal?"
    (let [cases
          [["1.99" true]
           ["-1.99" true]
           ["99" true]
           ["-99" true]
           ["99u" true]
           ["-99U" true]
           ["99L" true]
           ["-99l" true]
           ["unsigned int V1 = 2" false]
           ["char V1 = 261" false]
           ["2 + 261" false]
           ["-2 + 261" false]
           ["a + b" false]
           ]]
      (for [[code expected] cases]
        (testing (str code " -> " expected)
        (is (= expected (->> code parse-expr numeric-literal?))))))
      )
