(ns atom-finder.classifier-test
  (:require [clojure.test :refer :all]
            [schema.test]
            [atom-finder.util :refer :all]
            [atom-finder.classifier :refer :all]
            [clj-cdt.clj-cdt :refer :all]
            ))

(use-fixtures :once schema.test/validate-schemas)

(deftest test-macro-expansion-atoms?
  (testing "Atoms should be found in macro definitions, but not in macro expansions"
    (test-atom-lines "macro-def-atoms.c" "<true>" (default-finder any-atom?))))

(deftest test-conditional-atom?
  (testing "conditional-atom? finds all atoms in snippet study code"
    (is (= true
           (->> "conditional.c" parse-resource
                (get-in-tree [0 2 1 0 1 1 0])
                conditional-atom?
                )))

    (is (= false
           (->> "conditional.c" parse-resource
                (get-in-tree [0 2 1 0 1 1])
                conditional-atom?
                )))

    (let [lines  (->> "conditional.c" parse-resource
                      (filter-tree conditional-atom?)
                      (map loc)
                      (map :line))]

      (is (= lines [4 28 28 28 61]))
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

(deftest test-reversed-subscript-atom?
  (testing "reversed-subscript-atom? finds all atoms in snippet study code"

    (let [lines  (->> "reversed-subscript.c" parse-resource
                      (filter-tree reversed-subscript-atom?)
                      (map loc)
                      (map :line))]

      (is (= lines [7 10 11 11 12 15 16 16 17 17 17 27 28 28 38 39 39 41 42
                    45 54 55 55 60 65 72 72 74 74 76 76 79 81 83]))
      ))

  (testing "individual reversed-subscript expressions"
    (let [cases
          [["'a'[\"abc\"]"    true ]
           ["\"abc\"['a']"    false]
           ["(1&&2)[\"abc\"]" true ]
           ["\"abc\"[1&&2]"   false]
           ["null[123]"       false]
           ["(1?2:'c')[null]" true ]
           ["0x100220[1]"     false]
           ["1[0x100220]"     false]
           ]]

      (doseq [[code sc?] cases]
        (testing (str "Is reversed-subscript-atom? - " code " - " sc?)
          (is (= (reversed-subscript-atom? (parse-expr code)) sc?))))
      )))

(deftest comma-operator-test
  (testing "small statements"
    (is (comma-operator-atom? (parse-expr "1,2")))
    (is (false? (comma-operator-atom? (parse-expr "int a,b;"))))
    (is (comma-operator-atom? (parse-expr "num1,num2")))

  (testing "comma-operator? finds all atoms in snippet study code"
    (test-atom-lines "comma-operator.c" "<true>" (default-finder comma-operator-atom?)))))

(deftest test-assignment-as-value-atom?
  (testing "assignment-as-value-atom? finds all atoms in snippet study code"
    (test-atom-lines "assignment-as-value.c" "<true>" (default-finder assignment-as-value-atom?))))

(deftest test-operator-precedence-atom?
  (testing "operator-precedence-atom? finds all atoms in snippet study code"
    (test-atom-lines "operator-precedence.c" "<true>" (default-finder operator-precedence-atom?)))

  (testing "operator-precedence-child?"
    (->> "a + b * c" parse-expr operator-precedence-atom? is)
    (->> "a + b * c" parse-expr operator-precedence-child? not is)
    (->> "a + b * c" parse-expr (get-in-tree [1]) operator-precedence-child? is)
    ))

(deftest test-implicit-predicate-atom?
  (testing "implicit-predicate-atom? finds all atoms in sample code"
    (test-atom-lines "implicit-predicate.c" "<true>" (default-finder implicit-predicate-atom?))))
