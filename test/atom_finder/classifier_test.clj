(ns atom-finder.classifier-test
  (:require [clojure.test :refer :all]
            [schema.test]
            [atom-finder.util :refer :all]
            [atom-finder.classifier :refer :all]
            ))

(use-fixtures :once schema.test/validate-schemas)

(deftest test-short-circuitable?
  (let [cases 
        [["1 && 2"    false]
         ["1 || 2"    false]
         ["0 && 1"    false]
         ["0 || 1"    false]
         ["0 && f()"  true]
         ["1 && f()"  true] ; TODO
         ["0 || f()"  true] ; TODO
         ["1 || f()"  true]
         ["0 && x++"  true]
         ["0 && (x+=1)"  true]
         ["1 && (x=1)"  true] ; TODO
         ["1 && f()"  true] ; TODO
         ["0 || f()"  true] ; TODO
         ["1 || f()"  true]
         ["1&&(2&&f())" true]
         ["1&&(2?f():3)"true]
         ["f() || 2"  false]
         ["{1 && x++;}" false]
         ["2"         false]
         ["f()"       false]
         ["1 + 2"     false]]]

    (doseq [[code sc?] cases]
      (testing (str "Is short-circuitable? - " code " - " sc?)
        (is (= (short-circuitable-expr? (parse-expr code)) sc?))))
      ))

(deftest test-mutatable?
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
        (testing (str "Is mutatable? - " code " - " sc?)
        (is (= (mutatable-expr? (parse-expr code)) sc?)))
      )))

(deftest test-logic-as-control-flow-atoms?
  (testing "logic-as-control-flow-atoms finds all atoms in snippet study code"
    (let [lines  (->> (resource-path "logic-as-control-flow.c")
                      tu
                      logic-as-control-flow-atoms
                      (map loc)
                      (map :line))]

      (is (= lines [5 30 51]))
    )))

(deftest test-conditional-atom?
  (testing "conditional-atom? finds all atoms in snippet study code"
    (is (= true 
           (->> "conditional.c" resource-path tu
                (get-in-tree [0 2 1 0 1 1 0])
                conditional-atom?
                )))

    (is (= false
           (->> "conditional.c" resource-path tu
                (get-in-tree [0 2 1 0 1 1])
                conditional-atom?
                )))

    (let [lines  (->> (resource-path "conditional.c")
                      tu
                      (atoms-in-tree conditional-atom?)
                      (map loc)
                      (map :line))]

      (is (= lines [4 28 28 28 61]))
    )))

(deftest preprocessors-in-context-test
  (testing "Macro entirely in context"
    (let [filename (resource-path "macro-in-expression.c")
          atoms (preprocessors-in-contexts all-preprocessor non-toplevel-classifier (tu filename))]

      (is (= (map :line atoms) '(5 8 11)))
      ))

  (testing "Macro starts in context"
    (let [filename (resource-path "if-starts-in-expression.c")
          atoms (preprocessors-in-contexts all-preprocessor non-toplevel-classifier (tu filename))]

      (is (= (map :line atoms) '(9 11 14 16 18 23))) ; technically 25 should be here too because otherwise it's dependent on which if branch is evaluated
      ))

  (testing "Macro applied in function"
    (let [filename (resource-path "macro-application.c")
          atoms (preprocessors-in-contexts all-preprocessor non-toplevel-classifier (tu filename))]

      (is (= (map :line atoms) '()))
      ))

  (testing "Preprocessor from snippet study"
    (let [filename (resource-path "define-in-if-loop.c")
          atoms (preprocessors-in-contexts all-preprocessor statement-expression-classifier (tu filename))]

      (is (= (map :line atoms) '(4 7 11)))
      ))

  (testing "preprocesor-in-statement classifier"
    (let [pisc (-> atom-lookup (strict-get :preprocessor-in-statement) :classifier)]
      (is (true? (pisc (parse-expr "1 + \n#define M\n 3"))))
      (is (false? (pisc (parse-stmt "1 + \n#define M\n 3;"))))
      (is (false? (pisc (parse-stmt "{ 1 + 3; f(); }"))))
      )))

(deftest literal-encoding-test
  (testing "utils"
    (->>
     [[:dec "1234"] [:oct "01234"] [:hex "0x1234"] [:bin "0b1234"]]
     (map (fn [[k s]]
            (is (= k (radix s)))
            (is (= k (radix (parse-expr s))))
            ))
    )))
