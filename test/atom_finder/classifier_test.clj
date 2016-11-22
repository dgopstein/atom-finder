(ns atom-finder.classifier-test
  (:require [clojure.test :refer :all]
            [atom-finder.util :refer :all]
            [atom-finder.classifier :refer :all]
            ))

(deftest test-short-circuitable?
  (testing "Nodes that can short-circuit"
    (let [cases 
          [["1 && 2"    true]
           ["1 || 2"    true]
           ["0 && 1"    true]
           ["0 || 1"    true]
           ["1 && f()"  true]
           ["1&&2&&f()" true]
           ["1&&2?f():3"true]
           ["f() || 2" false]
           ["1 && 2;"  false]
           ["2"        false]
           ["f()"      false]
           ["1 + 2"    false]]]

          (doseq [[code sc?] cases]
            (is (= (short-circuitable-expr? (parse-expr code)) sc?)))
    )))

(deftest test-mutatable?
  (let [cases 
        [["f()"     true]
         ["a = 2"   true]
         ["a = b"   true]
         ["a = a"   true] ; TODO This should be false, but that's work for later
         ["a &= 3"  true]
         ["a++"     true]
         ["--a"     true]
         ["0?2:--a" true]
         ["1?2:--a" true] ; TODO should be false
         ["1 && 2;" false]
         ["2"       false]
         ["1 == 2"  false]
         ["1 | 2"   false]
         ["1 || 2"  false]
         ["~1"      false]
         ["+1"      false]
         ["1 + 2"   false]]]

      (doseq [[code sc?] cases]
        (testing (str "Is mutable? - " code " - " sc?)
        (is (= (mutatable-expr? (parse-expr code)) sc?)))
      )))
