(ns atom-finder.literal-encoding-test
  (:require [clojure.test :refer :all]
            [schema.test]
            [atom-finder.util :refer :all]
            [atom-finder.classifier-util :refer :all]
            [atom-finder.classifier :refer :all]
            ))

(use-fixtures :once schema.test/validate-schemas)

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

(deftest test-literal-encoding-atom?
  (testing "Is this a literal encoding atom?"
    (let [cases [
            ["1 + 2"     false]
            ["1 & 2"     false]
            ["-1 & 2"    false]
            ["1 & -2"    false]
            ["9 & 2"     true]
            ["1 & 02"    false]
            ["9 & 02"    true]
            ["-9 & 02"   true]
            ["01 & 02"   false]
            ["0x1 & 02"  false]
            ["0x1 & 2"   false]
            ["0x1 & 9"   true]
            ["1 & 0x2"   false]
            ["9 & 0x2"   true]
            ["~1"        false]
            ["-~1"       false]
            ["~8"        true]
            ["~-8"       true]
            ["~9"        true]
            ["~011"      false]
            ["~0x11"     false]
            ["!0x11"     false]
            ["!11"       false]
            ["8 << 2"    true]
            ["2 << 8"    false]
            ["#define X 1 << Y\nX" false]
            ["#define X 9 << Y\nX" true]
                 ]]

      (doseq [[expr expected] cases]
        (testing (str "Is this a literal encoding atom: " expr " - " expected)
          (is (= expected (literal-encoding-atom? (parse-expr expr)))))))
    ))
