(ns atom-finder.questions-test
  (:require [clojure.test :refer :all]
            [atom-finder.ast-spec :refer :all]
            [clojure.spec :as s]
            [atom-finder.questions.code-age :refer :all]
            ))

(deftest code-age-test
  (testing "distributed-sample"
    (is (= [0 2 3 5] (distributed-sample 4 (range 6))))
    (is (= [0 1 2 3 4 5] (distributed-sample 9 (range 6))))
    (is (= [] (distributed-sample 0 (range 6))))
    (is (= [3] (distributed-sample 1 (range 6))))
    (is (= [0 5] (distributed-sample 2 (range 6))))
    (is (= [] (distributed-sample 2 [])))
    ))
