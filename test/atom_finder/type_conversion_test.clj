(ns atom-finder.type-conversion-test
  (:require [clojure.test :refer :all]
            [schema.test]
            [atom-finder.util :refer :all]
            [atom-finder.test-util :refer :all]
            [atom-finder.classifier-util :refer :all]
            [atom-finder.classifier :refer :all]
            ))

(use-fixtures :once schema.test/validate-schemas)

(deftest test-type-conversion
  (testing "Is a macro wrapped in a do-while loop?"
    (test-atom-lines "type-conversion.c" "<true>" (default-finder type-conversion-atom?)))

  (let [cases
        [["int V1 = 1.99" true]
         ["long int V1 = 1.99" true]
         ["int V1 = 2, V2 = 1.99" true]
         ["int V1 = 1" false]
         ["unsigned int V1 = -2" true]
         ["unsigned int V1 = 2" false]
         ["char V1 = 261" true]
         ["char V1 = 26" false]
         ["int *V1 = {3, 4, 1, 2}", false]
         ]]
    (for [[code expected] cases]
      (testing (str code " -> " expected)
      ;(is (= expected (->> code parse-expr type-conversion-atom?))))))
      (is (= expected (->> code parse-expr coercing-declaration))))))
    )
