(ns atom-finder.core-test
  (:require [clojure.test :refer :all]
            [atom-finder.util :refer :all]
            [atom-finder.find-atom :refer :all]
            [atom-finder.core :refer :all]
            ))

;(deftest a-test
;  (testing "FIXME, I fail."
;    (is (= 0 1))))

(deftest macros-in-context-test
  (testing "Macro entirely in context"
    (let [filename (resource-path "macro-in-expression.c")
          atoms (macros-in-contexts (translation-unit filename))]

      (is (= (map :line atoms) '(5 8 11)))
      ))

  (testing "Macro starts in context"
    (let [filename (resource-path "if-starts-in-expression.c")
          atoms (macros-in-contexts (translation-unit filename))]

      (is (= (map :line atoms) '(9 11 14 16 18 23))) ; technically 25 should be here too because otherwise it's dependent on which if branch is evaluated
      ))


  )
