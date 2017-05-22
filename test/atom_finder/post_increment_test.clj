(ns atom-finder.post-increment-test
  (:require [clojure.test :refer :all]
            [schema.test]
            [atom-finder.util.util :refer :all]
            [atom-finder.util.test-util :refer :all]
            [atom-finder.util.classifier-util :refer :all]
            [atom-finder.classifier :refer :all]
            ))

(use-fixtures :once schema.test/validate-schemas)

(deftest test-post-*crement-atom?
  (testing "post-*crement? finds all atoms in c file"
    (test-atom-lines "post-increment.c" "<true>" (default-finder post-*crement-atom?)))
  )
