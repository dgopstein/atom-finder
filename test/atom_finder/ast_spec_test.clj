(ns atom-finder.ast-spec-test
  (:require [clojure.test :refer :all]
            [atom-finder.ast-spec :refer :all]
            [clojure.spec :as s]
            ))

(deftest count-nodes-test
  (testing "spec for literal-expression"
     (is (= 10 (count (s/exercise :atom-finder.ast-spec/literal-expression 10))))
    ))
