(ns atom-finder.macro-operator-precedence-test
  (:require [clojure.test :refer :all]
            [schema.test]
            [atom-finder.util :refer :all]
            [atom-finder.classifier :refer :all]
            )
  (:use     [clojure.pprint :only [pprint print-table]]))

(use-fixtures :once schema.test/validate-schemas)

(deftest test-macro-operator-precedence?
  (testing "Is a macro expanded unsafely into code outside of it?"
    (test-atom-lines "macro-operator-precedence.c" "<outer-atom>"
                     macro-outer-precedence-finder))

  (testing "Are macro arguments expanded unsafely inside the macro"
    (test-atom-lines "macro-operator-precedence.c" "<inner-atom>"
                     macro-inner-precedence-finder))

  (testing "Are macro arguments expanded unsafely inside the macro"
    (test-atom-lines "macro-operator-precedence.c" "-atom>"
                     macro-operator-precedence-finder))

  (testing "expansion-args-tree"
    (let [cases [
                 [["5 < 4" "7 > 2"] "#define M2(x,y) x+y+1 \n 3%(M2(5<4,7>2))"]
                 [["5 < 4"]         "#define M2(x,y) x+y+1 \n 3%(M2(5<4))"]
                 [[]                "#define M2 x+y+1 \n 3%(M2(5<4))"]
                 [[""]              "#define M2(x) x+y+1 \n 3%(M2())"]
                 ]]
      (for [[expected code] cases]
        (is (= expected (->> code parse-frag root-ancestor .getMacroExpansions first expansion-args-tree (map write-ast))) code))))

  (testing "expansion-args-str"
    ;; TODO We should be able to tokenize macro arguments better
    '(is (= ["ret == REDIS_OK &&
        ((redisReply*)reply)->type == REDIS_REPLY_ARRAY &&
        ((redisReply*)reply)->elements == 0"]

           (->> "#define test_cond(_c) if(_c) printf(\"033[0;32mPASSED033[0;0mn\"); else {printf(\"033[0;31mFAILED033[0;0mn\"); fails++;}
      test_cond(ret == REDIS_OK &&
        ((redisReply*)reply)->type == REDIS_REPLY_ARRAY &&
        ((redisReply*)reply)->elements == 0);"
                parse-frag root-ancestor .getMacroExpansions first
                expansion-args-str))))
  ;(testing "macro-body-str"
  ;  (->> "#define N	1^x*2
  ;    #define M(x)  N(x+3)
  ;    int y = M(4);"
  ;   parse-source
  ;   root-ancestor .getMacroExpansions first
  ;   macro-replace-arg-str write-ast
  ;   (= "1 ^ 4 * 2(4 + 3)")
  ;   is
  ;  ))

  (testing "parse-macro-def"
    (let [cases [["#define x 1"      {:name "x" :args nil :body "1"}]
                 ["#define x() 1"   {:name "x" :args [] :body "1"}]
                 ["#define x(y) 1"   {:name "x" :args ["y"] :body "1"}]
                 ["#define x(y,z) 1" {:name "x" :args ["y" "z"] :body "1"}]
                 ]]
      (doseq [[src res] cases]
        (->> src parse-source .getMacroDefinitions first
              parse-macro-def (= res) is))))
  )
