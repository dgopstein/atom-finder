(ns atom-finder.util-test
  (:require [clojure.test :refer :all]
            [atom-finder.util :refer :all]
            )
  (:import
           [org.eclipse.cdt.core.dom.ast IASTLiteralExpression]))

(deftest cdt-util-test
  (deftest count-nodes-test
    (testing "count-nodes"
      (is (= 3 (count-nodes (parse-expr "1 + 2"))))
      (is (= 6 (count-nodes (parse-expr "f(1 + 2)"))))
      ))

  (deftest parse-expr-stmt-test
    (testing "parse-expr/parse-stmt"
      (let [sanitize #(clojure.string/replace % #"\s+" " ")]
        (is (= "1 + 3" (sanitize (write-ast (parse-expr "1 + 3")))))
        (is (= "1 + 3; " (sanitize (write-ast (parse-stmt "1 + 3;")))))
        (is (= "{ 1 + 3; f(); } " (sanitize (write-ast (parse-stmt "{1 + 3; f();}")))))
        ))

    (testing "stmt-str?"
      (is (false? (stmt-str? "1 + 3")))
      (is (true? (stmt-str? "1 + 3;")))
      (is (true? (stmt-str? "{ 1 + 3; f(); } ")))
      )
    )

  (deftest parse-frag-test
    (testing "parse-expr"
      (let [sanitize #(clojure.string/replace % #"\s+" " ")
            cmp-frag (fn [parser code] (is (= (write-ast (parser code)) (write-ast (parse-frag code)))))]
        (cmp-frag parse-expr "1 + 3")
        (cmp-frag parse-stmt "1 + 3;")
        (cmp-frag parse-stmt "{ 1 + 3; f(); } ")
      )))

  (testing "height"
    (is (= 3 (->> "1 + 2;" parse-frag height)))
    (is (= 2 (->> "1 + 2" parse-frag height))))

  (testing "depth"
    (let [root (parse-source "int main() { 1 + 2; }")]
      (is (= 1 (->> root depth)))
      (is (= 1 (->> root safe-parent depth))) ; there is no parent
      (is (= 2 (->> root children first depth)))
      (is (= 3 (->> root children first children first depth)))
      (is (= 6 (->> root (get-in-tree [0 2 0 0 0]) depth)))
      ))
  )

(deftest sym-diff-test
  (testing "sym-diff"
    (is (= #{1 4} (sym-diff #{1 2 3} #{2 3 4})))
    (is (= #{1 2 3} (sym-diff #{1 2 3} #{})))
    (is (= #{} (sym-diff #{} #{})))
    (is (= #{} (sym-diff #{1 2 3} #{1 2 3})))
    (is (= #{} (sym-diff #{1 2 3} #{1 2 3} #{1 2 3})))
    (is (= #{1 2 3 4} (sym-diff #{1 2} #{2 3} #{3 4})))
    (is (= #{1 2 4 5} (sym-diff #{1 2 3} #{2 3 4} #{3 4 5})))
    ))

(deftest filter-tree-test
  (testing "filter-tree"
    (is
     (= ["1" "2" "3"]
        (->> "1 + 2 - 3" parse-expr
             (filter-tree (partial instance? IASTLiteralExpression))
             (map write-ast))))
    ))

(deftest clj-util-test

  (testing "flatcat"
    (is (= #{1 2} (set (flatcat [1] [2]))))
    (is (= #{1 2} (set (flatcat  1  [2]))))
    (is (= #{1 2} (set (flatcat [1]  2 ))))
    (is (= #{1 2} (set (flatcat  1   2 ))))
    (is (= #{[1] 2} (set (flatcat [[1]] 2))))
    )

  (testing "flatten1"
    (is (= [1 2 3] (flatten1 [[1 2] [3]])))
    (is (= [1 [2] 3] (flatten1 [[1 [2]] [3]]))))

  (testing "line-range"
    (let [cases [
                 [[]  [1 1] "a\nb\nc\n"]
                 [["a"] [1 2] "a\nb\nc\n"]
                 [["a" "b"] [1 3] "a\nb\nc\n"]
                 [["a" "b" "c"] [1 4] "a\nb\nc\n"]
                 [["b" "c"] [2 4] "a\nb\nc\n"]
                 [["b" "c"] [2 5] "a\nb\nc\n"]
                 [[] [6 6] "a\nb\nc\n"]
                 ]]

      (doseq [[expected [min max] s] cases]
        (is (= expected (line-range min max s))
            [expected (str "'" [min max] "' '" s "'")])
      ))
    )

  (testing "map-values"
    (is (= {1 1 2 4 3 9} (map-values #(* %1 %1) {1 1 2 2 3 3})))
    (is (= {1 "1a" 2 "2b" 3 "3c"} (map-values-kv #(str %1 %2) {1 \a 2 \b 3 \c})))
    )

  (testing "map-keys"
    (is (= {1 1 4 2 9 3} (map-keys #(* %1 %1) {1 1 2 2 3 3}))))

  (testing "file-ext"
    (let [cases [
                 [nil "gcc/ChangeLog"]
                 ["c" "gcc/random.c"]
                 ["cpp" "gcc/random.cpp"]
                 [nil "gcc/.gitignore"]
                 [nil ".gitignore"]
                 ["txt" "gcc/.gitignore.txt"]
                 ["txt" ".gitignore.txt"]
                 ]]

      (doseq [[expected filename] cases]
        (is (= expected (file-ext filename)) (list "=" expected filename)))
    ))

  (testing "dissoc-by"
    (is (= (dissoc-by #(.contains (first %1) "bad")
                      {"good1" 1, "bad1" 1, "good2" 2, "bad2" 2})
           {"good1" 1, "good2" 2}))
    )

  (testing "if-let*"
    (is (= 2 (if-let* [a 1] 2 3)))
    (is (= 3 (if-let* [a nil] 2 3)))
    (is (= 2 (if-let* [a 1 b 4] 2 3)))
    (is (= 3 (if-let* [a nil b 4] 2 3)))
    (is (= 3 (if-let* [a 1 b nil] 2 3)))
    (is (= 3 (if-let* [a nil b nil] 2 3)))
    (is (= 3 (if-let* [a false b true] 2 3)))
    (is (= 2 (if-let* [a true b true c true] 2 3)))
    (is (= 3 (if-let* [a true b true c nil] 2 3)))
    )
  )

(deftest writer-util-test
  (testing "write-node"
    (let [cases [
                 ["+" "a + b"]
                 [";" "a + b;"]
                 ["<IdExpression>" "a"]
                 ["b" "b" [0]]
                 ["int" "int a;" [0 0]]
                 ["()" "f(1)"]
                 ["f" "f(1)" [0 0]]
                 ["3" "3"]
                 ["*" "*c"]
                 ["++" "d++"]
                 ["?:" "1 ? 2 : 3"]
                 ["<ProblemStatement>" "~a+"]
                 ["=" "x = 1"]
                 ["=" "x = {}"] ; gcc/libstdc++-v3/testsuite/21_strings/basic_string_view/operations/find/wchar_t/2.cc:149
                 ]]

      (doseq [[expected frag idx] cases]
        (is (= expected (->> frag parse-frag ((if idx (partial get-in-tree idx) identity)) write-node)) (prn-str [expected frag])))
      ))
  )

(deftest keyword-test
  (testing "join-keywords"
    (is (= :abc_efg (join-keywords "_" [:abc :efg])))
    (is (= :abc_efg (join-keywords "_" ["abc" :efg])))
    (is (= :abc_efg (join-keywords "_" [:abc "efg"])))
    (is (= :abc_efg_hij (join-keywords "_" [:abc "efg" :hij])))
    (is (= :abcefghij (join-keywords [:abc "efg" :hij])))
  ))
