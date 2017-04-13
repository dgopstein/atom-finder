(ns atom-finder.classifier-util-test
  (:require [clojure.test :refer :all]
            [schema.test]
            [atom-finder.util :refer :all]
            [atom-finder.test-util :refer :all]
            [atom-finder.classifier-util :refer :all]
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
          (is (= base (radix literal))))))
    )

  (testing "Radix shouldn't classify non-numeric contacts"
    (let [cases [
           ["123"   :dec]
           ["012"   :oct]
           ["0x2"   :hex]
           ["0b1"   :bin]
           ["-123"  :dec]
           ["-012"  :oct]
           ["-0x2"  :hex]
           ["-0b1"  :bin]
           ["+12"   :dec]
           ["\"123\"" :non-numeric]
           ["true"    :non-numeric]
           ["'a'"     :non-numeric]
           ["a"       :non-literal]
           ["(12)"    :non-numeric]
           ["&12"     :non-numeric]
           ["~12"     :non-numeric]
         ]]
      (doseq [[literal base] cases]
        (testing (str "Which base is " literal " - " base)
          (is (= base (radix (parse-expr literal))))))
    ))
  )

(deftest test-numeric-literal?
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
        (is (= expected (->> code parse-expr numeric-literal?)))))))
      )

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

      (for [[expr expected] cases]
        (testing (str "Parse numeric literal from string: " expr " - " expected)
          (is (close? 0.001 expected (parse-numeric-literal (parse-expr expr)))))))
    ))

(deftest test-intersects-line-range?
  (testing "Get numeric value from a string"
    (let [src
          (clojure.string/join "\n" [
                                     "int main() {"
                                     "  int a = 0;"
                                     "  if (x > 1) {"
                                     "    a += 1;"
                                     "  }"
                                     "}"])
          root (parse-source src)
          cases [[false {:lines [1 1] :node [0 1]}] ; ranges are half-open so [1 1) is empty
                 [true  {:lines [1 2] :node [0 1]}]
                 [true  {:lines [1 2] :node [0]}]
                 [false {:lines [1 2] :node [0 2 0]}]
                 [true  {:lines [4 5] :node [0 2 1 1 0]}]
                 [true  {:lines [2 6] :node [0 2 1 1 0]}]
                 ]]

      (doseq [[expected {[start-line end-line] :lines node-path :node}] cases]
        (is (= expected (intersects-line-range? (get-in-tree node-path root) start-line end-line))
            [expected {:line [start-line end-line] :node node-path}]))
      )))
