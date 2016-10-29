(ns atom-finder.core-test
  (:require [clojure.test :refer :all]
            [atom-finder.core :refer :all]
            [atom-finder.find-atom :refer :all]
            ))

;(deftest a-test
;  (testing "FIXME, I fail."
;    (is (= 0 1))))



(deftest macros-in-context-test
  (testing "Macro in context"
    (let [filename (.getPath (clojure.java.io/resource "macro-in-expression.c"))
          atoms (macros-in-contexts (translation-unit filename))]

      (is (= (map :line atoms) '(8)))
      )))
