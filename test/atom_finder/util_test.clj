(ns atom-finder.util-test
  (:require [clojure.test :refer :all]
            [atom-finder.util :refer :all]
            ))

(deftest count-nodes-test
  (testing "count-nodes"
    (is (= 3 (count-nodes (parse-expr "1 + 2"))))
    (is (= 6 (count-nodes (parse-expr "f(1 + 2)"))))
    ))
