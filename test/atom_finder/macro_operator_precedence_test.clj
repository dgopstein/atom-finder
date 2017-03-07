(ns atom-finder.test.macro-operator-precedence-test
  (:require [clojure.test :refer :all]
            [schema.test]
            [atom-finder.util :refer :all]
            [atom-finder.test-util :refer :all]
            [atom-finder.classifier :refer :all]
            ))

(use-fixtures :once schema.test/validate-schemas)

(deftest test-macro-operator-precedence?
  (testing "Is a macro multi-line?"
    (test-atom-lines "macro-operator-precedence.c" "<multiline>" all-multiline-macros))
    )

  (testing "Find dangerous macro definitions"
    (test-atom-lines "macro-operator-precedence.c" "<def-atom>" (default-finder macro-operator-precedence-atom?)))
    )
