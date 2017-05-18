(ns atom-finder.util-test
  (:require [clojure.test :refer :all]
            [atom-finder.util :refer :all]
            )
  (:import
           [org.eclipse.cdt.core.dom.ast IASTLiteralExpression]))

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

(deftest util-test
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
  )
