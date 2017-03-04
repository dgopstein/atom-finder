(ns atom-finder.literal-encoding-test
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
          ["0X2"   :hex]
          ["0B1"   :bin]
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
      ))

  (testing "Radix shouldn't classify non-numeric contacts"
    (let [cases [
           ["123"   :dec]
           ["012"   :oct]
           ["0x2"   :hex]
           ["0b1"   :bin]
           ["\"123\"" :non-numeric]
           ["true"    :non-numeric]
           ["'a'"     :non-numeric]
           ["a"       :non-literal]
         ]]
      (doseq [[literal base] cases]
        (testing (str "Which base is " literal " - " base)
          (is (= base (radix (parse-expr literal))))))
    ))
  )

(deftest test-bitwise-op?
  (testing "Is this ASTNode a bitwise operation"
    (let [cases [
            ["1 + 2"     false]
            ["1 & 2"     true]
            ["'a' & 'b'" true]
            ["1 && 2"    false]
            ["1 &= 2"    true]
            ["1 << 2"    true]
            ["\"abc\" << \"def\"" true]
            ["~1"        true]
            ["!1"        false]
            ["1?2:3"     false]
            ["1"         false]
                 ]]

      (doseq [[expr expected] cases]
        (testing (str "Is this operator bitwise: " expr " - " expected)
          (is (= expected (bitwise-op? (parse-expr expr)))))))
    ))

(deftest test-parse-numeric-literal?
  (testing "Get numeric value from a string"
    (let [cases [
            ["11"    11]
            ["011"   9]
            ["0x11"  17]
            ["0b11"  3]
            ["-11"   -11]
            ["-011"  -9]
            ["-0x11" -17]
            ["-0b11" -3]
            ["-0B11" -3]
            ["0x10.1p2"   64.250000]
            ["010e1"      100.000000]
            ["0x10.1e3p2" 64.471680]
            ["0x10e3.1p2" 17292.250000]
            ["010.1e1"    101.000000]
            ["12ul"  12]
            ["012l"  10]
            ["0x12u" 18]
            [".012" 0.012]
                 ]]

      (doseq [[expr expected] cases]
        (testing (str "Parse numeric literal from string: " expr " - " expected)
          (is (close? 0.001 expected (parse-numeric-literal expr))))))
    ))

(deftest test-numeric-literal-parsing?
  (testing "Find the value of numeric literal strings"
    (let [cases [
            ["1 + 2"     false]
            ["1 & 2"     false]
            ["9 & 2"     true]
            ["1 & 02"    false]
            ["9 & 02"    true]
            ["01 & 02"   false]
            ["0x1 & 02"  false]
            ["0x1 & 2"   false]
            ["0x1 & 9"   true]
            ["1 & 0x2"   false]
            ["9 & 0x2"   true]
            ["~1"        false]
            ["~8"        true]
            ["~9"        true]
            ["~011"       false]
            ["~0x11"      false]
            ["!0x11"      false]
            ["!11"        false]
                 ]]

      (doseq [[expr expected] cases]
        (testing (str "Is this a literal encoding atom: " expr " - " expected)
          (is (= expected (literal-encoding-atom? (parse-expr expr)))))))
    ))
