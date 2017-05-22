(ns atom-finder.type-conversion-test
  (:require [clojure.test :refer :all]
            [schema.test]
            [atom-finder.util.util :refer :all]
            [atom-finder.util.test-util :refer :all]
            [atom-finder.util.classifier-util :refer :all]
            [atom-finder.classifier :refer :all]
            ))

(use-fixtures :once schema.test/validate-schemas)

(deftest test-type-conversion
  (testing "Is a type-conversion atom here?"
    (test-atom-lines "type-conversion.c" "<true>" (default-finder type-conversion-atom?)))

  (let [cases
        [["int V1 = 1.99" true]
         ["long int V1 = 1.99" true]
         ["int V1 = 2, V2 = 1.99" true]
         ["int V1 = 1" false]
         ["unsigned int V1 = -2" true]
         ["unsigned int V1 = 2" false]
         ["char V1 = 261" true]
         ["int V1 = 261" false]
         ["char V1 = 129" true]
         ["char V1 = 127 + 5" false] ; maybe one day
         ["char V1 = 26" false]
         ["int *V1 = {3, 4, 1, 2}", false]
         ["int V1" false]
         ]]
    (for [[code expected] cases]
      (testing (str code " -> " expected)
      ;(is (= expected (->> code parse-expr type-conversion-atom?))))))
      (is (= expected (->> code parse-expr type-conversion-atom?))))))
    )
