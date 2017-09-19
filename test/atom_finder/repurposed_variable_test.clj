(ns atom-finder.repurposed-variable-test
  (:require [clojure.test :refer :all]
            [schema.test]
            [atom-finder.util :refer :all]
            [atom-finder.classifier :refer :all]
            ))

(use-fixtures :once schema.test/validate-schemas)

(deftest repurposed-variable-test
  (testing "Is a type-conversion atom here?"
    (test-atom-lines "repurposed-variable.c" "<true>" repurposed-variable-atoms)))

