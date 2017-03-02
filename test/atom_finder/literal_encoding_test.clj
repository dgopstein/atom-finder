(ns atom-finder.classifier-test
  (:require [clojure.test :refer :all]
            [schema.test]
            [atom-finder.util :refer :all]
            [atom-finder.classifier :refer :all]
            ))

(use-fixtures :once schema.test/validate-schemas)

(deftest test-radix
  (testing "Determine the radix of string literals"
    (let [cases [
          ["123"   :dec]
          ["012"   :oct]
          ["0x2"   :hex]
          ["0b1"   :bin]
          ["0b0"   :bin]
          ["0"     :dec]
          ["0.0"   :dec]
          ["0.1"   :dec]
          [".0"    :dec]
          [".1"    :dec]
          ["0x0"   :hex]
          ["00"    :oct]
          ["0123.456" :dec]
          ["0x123.456p0" :hex]
          ["0x123.456p1" :hex]
          ["0x123.456p2" :hex]
          ["012e2" :dec]
          ["12ul"  :dec]
          ["012l"  :oct]
          ["0x12u" :hex]
                 ]
          signed-cases (mapcat (fn [[s b]] [[s b]
                                          [(str "-" s) b]
                                          [(str "+" s) b]]) cases)
          ]

    (doseq [[literal base] signed-cases]
      (testing (str "Which base is " literal " - " base)
        (is (= base (radix literal)))))

      )))
