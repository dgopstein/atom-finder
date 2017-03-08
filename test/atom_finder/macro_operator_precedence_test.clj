(ns atom-finder.macro-operator-precedence-test
  (:require [clojure.test :refer :all]
            [schema.test]
            [atom-finder.util :refer :all]
            [atom-finder.test-util :refer :all]
            [atom-finder.classifier :refer :all]
            ))

(use-fixtures :once schema.test/validate-schemas)

(deftest test-macro-operator-precedence?
  (testing "Is a macro wrapped in a do-while loop?"
    (test-atom-lines "macro-operator-precedence.c" "<do-wrapped>" all-do-wrapped-macros))

  (testing "Are macro parameter wrapped in parens?"
    (test-atom-lines "macro-operator-precedence.c" "<param-wrapped>" all-param-wrapped-macros))

  (let [cases
        [["#define f4(x) do { x; } while(0)" false]
         ["#define f(x) x?x:x" false]
         ["#define f2(x) x?x:x" false]
         ["#define f5(x) (x)?x:x" false]
         ["#define f6(x) (x)?(x):(x)" true]
         ["#define f3 x?x:x" true]
         ["#define Z3(x, y) x*y" false]
         ["#define Z3(x, y) x*(y)" false]
         ["#define Z3(x, y) (x)*y" false]
         ["#define Z3(x, y) (x)*(y)" true]
         ]]
    (for [[code expected] cases]
      (testing (str code " -> " expected)
      (is (= expected (->> code parse-source macro-definitions first param-wrapped-macro?))))))

  (testing "Find dangerous macro definitions"
    (test-atom-lines "macro-operator-precedence.c" "<def-atom>" macro-operator-precedence-atoms))
    )
