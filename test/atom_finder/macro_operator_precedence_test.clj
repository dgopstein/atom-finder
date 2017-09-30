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
                     macro-outer-precedence-finder)))
