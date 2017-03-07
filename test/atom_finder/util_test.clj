(ns atom-finder.util-test
  (:require [clojure.test :refer :all]
            [atom-finder.util :refer :all]
            ))

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
    (let [sanitize #(clojure.string/replace % #"\s+" " ")]
      (let [c "1 + 3"]            (is (= (write-ast (parse-expr c)) (write-ast (parse-frag c)))))
      (let [c "1 + 3;"]           (is (= (write-ast (parse-stmt c)) (write-ast (parse-frag c)))))
      (let [c "{ 1 + 3; f(); } "] (is (= (write-ast (parse-stmt c)) (write-ast (parse-frag c)))))
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

