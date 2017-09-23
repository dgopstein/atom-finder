(ns atom-finder.classifier-util-test
  (:require [clojure.test :refer :all]
            [schema.test]
            [atom-finder.util :refer :all]
            [atom-finder.classifier :refer :all]
            [clojure.pprint :refer [pprint]]
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

(deftest test-integeral?
  (testing "Is a number an integer"
    (let [cases [
            ["11"    true]
            ["011"   true]
            ["0x11"  true]
            ["0b11"  true]
            ["-11"   true]
            ["-011"  true]
            ["-0x11" true]
            ["-0b11" true]
            ["-0B11" true]
            ["0x10.1p2"   false]
            ["010e1"      false]
            ["0x10.1e3p2" false]
            ["0x10e3.1p2" false]
            ["010.1e1"    false]
            ["12ul"  true]
            ["012l"  true]
            ["0x12u" true]
            [".012" false]
            ["0x756e6547" true]
                 ]]

      (for [[expr expected] cases]
        (testing (str "Parse numeric literal from string: " expr " - " expected)
          (is (= (integral? expr) expected)))))
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
            ["0x756e6547" 1970169159]
            ["0xffff800000000000" 18446603336221196288]
                 ]]
      (for [[expr expected] cases]
        (testing (str "Parse numeric literal from string: " expr " - " expected)
          (let [res (parse-numeric-literal (parse-expr expr))]
            (is (or (= expected (pprn res)) (close? 0.001 expected res)))))))
    ))

(deftest test-intersects-line-range?
  (let [src
        (clojure.string/join "\n" [
                                   "int main() {"
                                   "  int a = 0;"
                                   "  if (x > 1) {"
                                   "    a += 1;"
                                   "  }"
                                   "}"])
        root (parse-source src)]

    (testing "line range stuff"
      (let [cases
            [
             [{:parent? false :intersects? false :contained-by? false :contains? false}
              {:lines [1 1] :node [0 1]}] ; ranges are half-open so [1 1) is empty
             [{:parent? false :intersects? true  :contained-by? true  :contains? false}
              {:lines [1 2] :node [0 1]}]
             [{:parent? false :intersects? true  :contained-by? false :contains? false}
              {:lines [1 2] :node [0]}]
             [{:parent? false :intersects? true  :contained-by? true  :contains? false}
              {:lines [1 8] :node [0]}]
             [{:parent? false :intersects? false :contained-by? false :contains? false}
              {:lines [1 2] :node [0 2 0]}]
             [{:parent? false :intersects? true  :contained-by? true  :contains? false}
              {:lines [2 3] :node [0 2 0]}]
             [{:parent? false :intersects? true  :contained-by? true  :contains? false}
              {:lines [2 3] :node [0 2 0]}]
             [{:parent? false :intersects? true  :contained-by? true  :contains? false}
              {:lines [4 5] :node [0 2 1 1 0]}]
             [{:parent? false :intersects? true  :contained-by? true  :contains? false}
              {:lines [2 6] :node [0 2 1 1 0]}]
             [{:parent? true  :intersects? true  :contained-by? false :contains? true }
              {:lines [2 6] :node [0 2]}]
             [{:parent? false :intersects? true  :contained-by? false :contains? true }
              {:lines [2 5] :node [0]}]
             ;; A node can only contain a line range if it exists on lines outside that range.
             ;; if a node exists on each, and is the first and last node on the first and last
             ;; lines, that's still not enough for containment. A change could be made on the
             ;; same line before/after the node: "x++;" -> "if (x < 3) x++;"
             [{:parent? false :intersects? true  :contained-by? false :contains? false}
              {:lines [3 4] :node [0 2 1]}]
             ]]

        (doseq [[{parent? :parent? intersects? :intersects? contained-by?
                  :contained-by? contains? :contains?}
                 {[start-line end-line] :lines node-path :node}] cases]
          (is (= intersects? (intersects-line-range? start-line end-line (get-in-tree node-path root)))
              ["intersects-line-range?" intersects? {:line [start-line end-line] :node node-path}])
          (is (= contained-by? (contained-by-line-range? start-line end-line (get-in-tree node-path root)))
              ["contained-by-line-range?" contained-by? {:line [start-line end-line] :node node-path}])
          (is (= parent? (line-range-parent? start-line end-line (get-in-tree node-path root)))
              ["line-range-parent?" parent? {:line [start-line end-line] :node node-path}])
          (is (= contains? (contains-line-range? start-line end-line (get-in-tree node-path root)))
              ["contains-line-range?" contains? {:line [start-line end-line] :node node-path}])
          )))

    ;; 1 int main() {
    ;; 2   int a = 0;
    ;; 3   if (x > 1) {
    ;; 4     a += 1;
    ;; 5   }
    ;; 6 }

    (testing "line range parent"
      (let [cases
            [[[0 2]     [3 4]]
             [[0 2 1 1] [4 4]]
             [[0 2 1 1] [4 5]]
             [[0 2]     [2 4]]
             ]]

        (doseq [[expected [min max]] cases]
          (is (= (some-> expected (get-in-tree root)) (line-range-parent min max root))))))

    ;(->> root (get-in-tree [0 2 1 1]) write-ast)
    )

  (testing "contains-offset?"
    (let [src-cases [[(->> "1 + 2" parse-frag) ; => {:offset 13, :length 5}
                      [[true 13]
                       [true 14]
                       [true 18]
                       [false 12]
                       [false 19]
                       [false 0]
                       [false -1]]]
                     [(->> "int a(void)" parse-expr (get-in-tree [1 1 1])) ; => {:offset 23, :length 0}
                      [[true 23]
                       [false 24]
                       [false 28]
                       [false 22]
                       [false 29]
                       [false 0]
                       [false -1]
                       ]]
                     [nil [[nil 23]]]]]
      (for [[node cases]   src-cases
            [expected offset] cases]
        (is (= expected (contains-offset? node offset))))
      )
    )

  (testing "offset-parent"
    (let [node (parse-source " int main() {\nint x = 0;\nif (x > 0) {\nx += 1;\n}\nreturn x;\n} ")
          cases [["IfStatement" 26]
                 ["IfStatement" 28]
                 ["Name" 29]
                 ["CompoundStatement" 59]
                 ["TranslationUnit" 0]
                 ["TranslationUnit" 60]
                 [nil 80000]
                 ]]
      (for [[expected offset] cases]
        (is (= expected (some-> node (offset-parent offset) typename)) (str "offset: " offset)))
      )
    )
  )

