(ns atom-finder.meaningful-change-test
  (:require [clojure.test :refer :all]
            [schema.test]
            [atom-finder.util :refer :all]
            [atom-finder.test-util :refer :all]
            [atom-finder.classifier :refer :all]
            [atom-finder.classifier-util :refer :all]
            [atom-finder.meaningful-change :refer :all]
            ))

(use-fixtures :once schema.test/validate-schemas)

(deftest test-meaningful-change
  (testing "meaningful changes are detected"
    (test-atom-lines "atom-comments.c" "<true>"
                     #(mapcat last (atom-finder-comments (:post-increment atom-lookup) %)))))

(deftest test-changed-comments
  (testing "meaningful changes are detected"
    (changed-comments
     (parse-resource "meaningful-change-before.c")
     (parse-resource "meaningful-change-after.c"))
    ))
