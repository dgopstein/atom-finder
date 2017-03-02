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

(deftest test-literal-encoding?
  (testing "Find examples of Literal Encoding atom"
    (let [cases [
            ["1 + 2"     false]
            ["1 & 2"     true]
            ["1 & 02"    true]
            ["01 & 02"   false]
            ["0x1 & 02"  false]
            ["0x1 & 2"   true]
            ["1 & 0x2"   true]
            ["~1"        true] ; Maybe values <8 shouldn't be the atom
            ["~8"        true]
            ["~01"       false]
            ["~0x1"      false]
            ["!0x1"      false]
            ["!1"        false]
                 ]]

      (doseq [[expr expected] cases]
        (testing (str "Is this a literal encoding atom: " expr " - " expected)
          (is (= expected (literal-encoding-atom? (parse-expr expr)))))))
    ))
