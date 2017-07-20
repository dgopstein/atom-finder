(ns atom-finder.tree-diff-test
  (:require [clojure.test :refer :all]
            [atom-finder.util :refer :all]
            [atom-finder.constants :refer :all]
            [atom-finder.patch :refer :all]
            [atom-finder.atom-patch :refer :all]
            [atom-finder.atom-stats :refer :all]
            [atom-finder.classifier :refer :all]
            [atom-finder.tree-diff :refer :all]
            [clj-jgit.porcelain  :as gitp]
            [clj-jgit.querying :as gitq]
            [clj-jgit.internal :as giti]
            [clojure.pprint :refer :all]
            ))

; Have lines that contain atoms been changed in a meaningful way?

(deftest atom-line-meaningful-change?-test
  (testing "Which lines AST differences are meaningful"
    (let [cases [
                 [false "b = 1 + a++" "b = 1 + a++"]
                 [false "b = 1 + a++" "b = 2 + a++"]
                 [true "b = 1 + a++" "b = 1 * a++"]
                 [true  "b = 1 + a++" "b = 1 + (a++)"]
                 ]]

      (doseq [[expected a b] cases]
        (is (= expected (meaningful-change? (parse-frag a) (parse-frag b)))
            [expected (str "'" a "' '" b"'")])
      ))))

(deftest tree=-test
  (testing "Which ASTs are equal"
    (let [cases [
                 [true "b = 1 + a++" "b = 1 + a++"]
                 [true " b  =  1 /*z*/  +  a++ //asf" "b = 1 + a++"]
                 [false "b = 1 + a++" "b = 2 + a++"]
                 [false "b = 1 + a++" "b = 1 * a++"]
                 [false  "b = 1 + a++" "b = 1 + (a++)"]
                 ]]

      (doseq [[expected a b] cases]
        (is (= expected (tree= (parse-frag a) (parse-frag b)))
            [expected (str "'" a "' '" b"'")])
      ))))

(deftest node=-test
  (testing "Which individual nodes are equal"
    (let [cases [
                 [true "b = 1 + a++" "b = 1 + a++"]
                 [true " b  =  1 /*z*/  +  a++ //asf" "b = 1 + a++"]
                 [false "b = 1 + a++" "b = 1 * a++"]
                 [false  "b = 1 + a++" "b *= 1 + a++"]
                 [false  "b += 1 + a++" "b *= 1 + a++"]
                 [true  "(1)" "(2)"]
                 [false  "(1)" "-1"]
                 [false  "+1" "-1"]
                 [true  "if (1) 2;" "if (2) 3;"]
                 [true  "2" "3"]
                 ]]

      (doseq [[expected a b] cases]
        (is (= expected (node= (parse-frag a) (parse-frag b)))
            [expected (str "'" a "' '" b"'")])
        ))))

(deftest atoms-removed-test
  (testing "Which atoms are removed between files"
    (let [a (parse-resource "atoms-removed-before.c")
          b (parse-resource "atoms-removed-after.c")
          atms-rmvd (atoms-removed (:finder (atom-lookup :post-increment)) a b)
          lines     (map start-line atms-rmvd)
          expected-lines (find-lines #"<true>" (resource-path "atoms-removed-before.c"))]

      (is (= expected-lines lines))
      )
    ))
