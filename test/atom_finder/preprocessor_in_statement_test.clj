(ns atom-finder.preprocessor-in-statement-test
  (:require [clojure.test :refer :all]
            [schema.test]
            [atom-finder.util :refer :all]
            [atom-finder.classifier :refer :all]
            ))

(use-fixtures :once schema.test/validate-schemas)

(deftest preprocessors-in-context-test
  (testing "Macro entirely in context"
    (let [ast (parse-resource "macro-in-expression.c")
          atoms (preprocessors-in-contexts all-preprocessor non-toplevel? ast)]

      (is (= (map :line atoms) '(5 8 11)))
      ))

  (testing "Macro starts in context"
    (let [ast (parse-resource "if-starts-in-expression.c")
          atoms (preprocessors-in-contexts all-preprocessor non-toplevel? ast)]

      (is (= (map :line atoms) '(9 11 14 16 18 23))) ; technically 25 should be here too because otherwise it's dependent on which if branch is evaluated
      ))

  (testing "Macro applied in function"
    (let [ast (parse-resource "macro-application.c")
          atoms (preprocessors-in-contexts all-preprocessor non-toplevel? ast)]

      (is (= (map :line atoms) '()))
      ))

  (testing "Preprocessor from snippet study"
    (let [ast (parse-resource "define-in-if-loop.c")
          atoms (preprocessors-in-contexts all-preprocessor stmt-or-expr? ast)]

      (is (= (map :line atoms) '(4 7 11)))
      ))

  (testing "preprocesor-in-statement classifier"
    (let [pisc (-> atom-lookup (strict-get :preprocessor-in-statement) :classifier)
          cases
          [["1 + \n#define M\n 3"   true]
           ["1 + \n#define M\n 3;" false]
           ["{ 1 + 3; f(); }"      false]
           ["1 \n#ifdef X\n + 3 \n#endif\n + 4" false]
            ]]
      (doseq [[code sc?] cases]
        (testing (str "preprocessor-in-statement: " code " -> " sc?)
          (is (= (pisc (parse-frag code)) sc?))))))

  (testing "preprocessor-in-statement? finds all atoms in code"
    (test-atom-lines "preprocessor-in-statement.c" "<true>" non-toplevel-defines))
)
