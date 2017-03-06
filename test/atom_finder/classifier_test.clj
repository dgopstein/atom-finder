(ns atom-finder.classifier-test
  (:require [clojure.test :refer :all]
            [schema.test]
            [atom-finder.util :refer :all]
            [atom-finder.classifier :refer :all]))
            

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
        (is (= (short-circuitable-expr? (parse-expr code)) sc?))))))
      

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
         (is (= (mutatable-expr? (parse-expr code)) sc?))))))
      

(deftest test-logic-as-control-flow-atoms?
  (testing "logic-as-control-flow-atoms finds all atoms in snippet study code"
    (let [lines  (->> (resource-path "logic-as-control-flow.c")
                      tu
                      logic-as-control-flow-atoms
                      (map loc)
                      (map :line))]

      (is (= lines [5 30 51])))))
    

(deftest test-conditional-atom?
  (testing "conditional-atom? finds all atoms in snippet study code"
    (is (= true 
           (->> "conditional.c" resource-path tu
                (get-in-tree [0 2 1 0 1 1 0])
                conditional-atom?)))
                

    (is (= false
           (->> "conditional.c" resource-path tu
                (get-in-tree [0 2 1 0 1 1])
                conditional-atom?)))
                

    (let [lines  (->> (resource-path "conditional.c")
                      tu
                      (atoms-in-tree conditional-atom?)
                      (map loc)
                      (map :line))]

      (is (= lines [4 28 28 28 61])))))
    

(deftest preprocessors-in-context-test
  (testing "Macro entirely in context"
    (let [filename (resource-path "macro-in-expression.c")
          atoms (preprocessors-in-contexts all-preprocessor non-toplevel-classifier (tu filename))]

      (is (= (map :line atoms) '(5 8 11)))))
      

  (testing "Macro starts in context"
    (let [filename (resource-path "if-starts-in-expression.c")
          atoms (preprocessors-in-contexts all-preprocessor non-toplevel-classifier (tu filename))]

      (is (= (map :line atoms) '(9 11 14 16 18 23))))) ; technically 25 should be here too because otherwise it's dependent on which if branch is evaluated
      

  (testing "Macro applied in function"
    (let [filename (resource-path "macro-application.c")
          atoms (preprocessors-in-contexts all-preprocessor non-toplevel-classifier (tu filename))]

      (is (= (map :line atoms) '()))))
      

  (testing "Preprocessor from snippet study"
    (let [filename (resource-path "define-in-if-loop.c")
          atoms (preprocessors-in-contexts all-preprocessor statement-expression-classifier (tu filename))]

      (is (= (map :line atoms) '(4 7 11)))))
      

  (testing "preprocesor-in-statement classifier"
    (let [pisc (-> atom-lookup (strict-get :preprocessor-in-statement) :classifier)]
      (is (true? (pisc (parse-expr "1 + \n#define M\n 3"))))
      (is (false? (pisc (parse-stmt "1 + \n#define M\n 3;"))))
      (is (false? (pisc (parse-stmt "{ 1 + 3; f(); }")))))))
      

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

    (let [lines  (->> (resource-path "reversed-subscript.c")
                      tu
                      (atoms-in-tree reversed-subscript-atom?)
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
    (is (comma-atom? (parse-expr "1,2")))
    (is (false? (comma-atom? (parse-expr "int a,b;"))))
    (is (comma-atom? (parse-expr "num1,num2"))))

  (testing "comma-atom? finds all atoms in snippet study code"
     (let [lines  (->> (resource-path "comma.c")
                      tu
                      (atoms-in-tree comma-atom?)
                      (map loc)
                      (map :line))]

      (is (= lines [4 11 11 11 28 29 31 36 36 37]))))
)

(deftest pre-increment-decrement-test
  (let [cases 
        [[parse-expr "a = ++b"     true]
         [parse-expr "cout << ++a"     true]
         [parse-expr "printf(\"All the stuffs\", ++a)"     true]
         [parse-expr "--a,b"     true]
         [parse-expr "(true)? ++a: --a" false]
         [parse-stmt "for (int i; i < 10; ++i){}" false]
         [parse-stmt "++i;"       false]
         [parse-stmt "i++;"  false]
         [parse-expr "a = b++"   false]]]

      (doseq [[action code sc?] cases]
        (testing (str "Contains confusing pre in/decrement? - " code " - " sc?)
         (is (= (pre-increment-decrement-atom? (action code)) sc?)))))

  (testing "pre-increment-decrement-atom? finds all atoms in snippet study code"
     (let [lines  (->> (resource-path "decrement-increment.c")
                      tu
                      (atoms-in-tree pre-increment-decrement-atom?)
                      (map loc)
                      (map :line))]

      (is (= lines [14 15 15 17 24 26])))))

(deftest post-increment-decrement-test
  (let [cases 
        [[parse-expr "a = b++"     true]
         [parse-expr "cout << a++"     true]
         [parse-expr "printf(\"All the stuffs\", a++)"     true]
         [parse-expr "a++,b"     true]
         [parse-expr "(true)? a++: a--" false]
         [parse-stmt "for (int i; i < 10; i++){}" false]
         [parse-stmt "++i;"       false]
         [parse-stmt "i++;"  false]
         [parse-expr "a = ++b"   false]]]

      (doseq [[action code sc?] cases]
        (testing (str "Contains confusing post in/decrement? - " code " - " sc?)
         (is (= (post-increment-decrement-atom? (action code)) sc?)))))

  (testing "post-increment-decrement-atom? finds all atoms in snippet study code"
     (let [lines  (->> (resource-path "decrement-increment.c")
                      tu
                      (atoms-in-tree post-increment-decrement-atom?)
                      (map loc)
                      (map :line))]

      (is (= lines [16 24 46])))))

(deftest test-omitted-curly-braces-atom?
  (testing "omitted-curly-braces-atom? finds all atoms in snippet study code"         

    (let [lines  (->> (resource-path "curly-braces.c")
                      tu
                      (atoms-in-tree curly-braces-atom?)
                      (map loc)
                      (map :line))]

      (is (= lines [3 4 7 8 26 27 28 29 41 43 44 54 66 79]
;;TODO: should be [3 4 4 7 8 26 27 28 29 29 41 43 44 45 54 66 79 92 93 95]
;;           Doesn't recognize else clause, doesn't recognize switch
;;Debatable: should it be [3 4 5 7 9 26 27 28 29 30 41 43 45 54 66 79 92 93 95]?

)))))

