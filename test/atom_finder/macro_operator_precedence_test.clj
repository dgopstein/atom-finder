(ns atom-finder.macro-operator-precedence-test
  (:require [clojure.test :refer :all]
            [schema.test]
            [atom-finder.util :refer :all]
            [atom-finder.classifier :refer :all]
            )
  (:use     [clojure.pprint :only [pprint print-table]]))

(use-fixtures :once schema.test/validate-schemas)

(deftest test-macro-operator-precedence?
  (testing "Is a macro expanded unsafely into code outside of it?"
    (test-atom-lines "macro-operator-precedence.c" "<outer-atom>"
                     macro-outer-precedence-finder))

  (testing "Are macro arguments expanded unsafely inside the macro"
    (test-atom-lines "macro-operator-precedence.c" "<inner-atom>"
                     macro-inner-precedence-finder))

  (testing "expansion-args-tree"
    (let [cases [
                 [["5 < 4" "7 > 2"] "#define M2(x,y) x+y+1 \n 3%(M2(5<4,7>2))"]
                 [["5 < 4"]         "#define M2(x,y) x+y+1 \n 3%(M2(5<4))"]
                 [[]                "#define M2 x+y+1 \n 3%(M2(5<4))"]
                 [[""]              "#define M2(x) x+y+1 \n 3%(M2())"]
                 ]]
      (for [[expected code] cases]
        (is (= expected (->> code parse-frag root-ancestor .getMacroExpansions first expansion-args-tree (map write-ast))) code))))
  )
