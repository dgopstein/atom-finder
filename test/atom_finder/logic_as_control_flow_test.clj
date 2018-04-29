(ns atom-finder.logic-as-control-flow-test
  (:require [clojure.test :refer :all]
            [schema.test]
            [atom-finder.util :refer :all]
            [atom-finder.classifier :refer :all]
            [clj-cdt.clj-cdt :refer :all]
            ))

(use-fixtures :once schema.test/validate-schemas)

(deftest test-short-circuitable?
  (let [cases
        [["1 && 2"    false]
         ["1 || 2"    false]
         ["0 && 1"    false]
         ["0 || 1"    false]
         ["0 && f()"  false]
         ["1 && f()"  false] ; TODO
         ["0 || f()"  false] ; TODO
         ["1 || f()"  false]
         ["0 && x++"  true]
         ["0 && (x+=1)"  true]
         ["1 && (x=1)"  true] ; TODO
         ["1 && f()"  false] ; TODO
         ["0 || f()"  false] ; TODO
         ["1 || f()"  false]
         ["1&&(2&&f())" false]
         ["1&&(2?f():3)"false]
         ["f() || 2"  false]
         ["{1 && x++;}" false]
         ["2"         false]
         ["f()"       false]
         ["1 + 2"     false]]]

    (doseq [[code sc?] cases]
      (testing (str "Is short-circuitable? - " code " - " sc?)
        (is (= (short-circuitable-expr? (parse-expr code)) sc?))))
      ))

(deftest test-maybe-mutatable?
  (let [cases
        [["f()"     true]
         ["a = 2"   true]
         ["a = b"   true]
         ["a = a"   true] ; TODO This should be false, but that's work for later
         ["a &= 3"  true]
         ["a += 3"  true]
         ["a++"     true]
         ["--a"     true]
         ["0?2:--a" true]
         ["1?2:--a" true] ; TODO should be false
         ["{1 && 2;}" false]
         ["2"       false]
         ["1 == 2"  false]
         ["1 | 2"   false]
         ["1 || 2"  false]
         ["~1"      false]
         ["+1"      false]
         ["1 + 2"   false]]]

      (doseq [[code sc?] cases]
        (testing (str "Is maybe mutatable? - " code " - " sc?)
        (is (= (maybe-mutatable-expr? (parse-expr code)) sc?)))
      )))

(deftest test-definitely-mutatable?
  (let [cases
        [["f()"     false]
         ["a = 2"   true]
         ["a = b"   true]
         ["a = a"   true] ; TODO This should be false, but that's work for later
         ["a &= 3"  true]
         ["a += 3"  true]
         ["a++"     true]
         ["--a"     true]
         ["0?2:--a" true]
         ["1?2:--a" true] ; TODO should be false
         ["{1 && 2;}" false]
         ["2"       false]
         ["1 == 2"  false]
         ["1 | 2"   false]
         ["1 || 2"  false]
         ["~1"      false]
         ["+1"      false]
         ["1 + 2"   false]]]

      (doseq [[code sc?] cases]
        (testing (str "Is definitely mutatable? - " code " - " sc?)
        (is (= (mutatable-expr? (parse-expr code)) sc?)))
      )))

(deftest test-logic-as-control-flow-atoms?
  (testing "logic-as-control-flow-atoms finds all atoms in snippet study code"
    (test-atom-lines "logic-as-control-flow.c" "<true>" logic-as-control-flow-atoms)))
