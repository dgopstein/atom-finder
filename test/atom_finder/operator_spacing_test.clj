(ns atom-finder.operator-spacing-test
  (:require [clojure.test :refer :all]
            [clj-cdt.clj-cdt :refer :all]
            [clj-cdt.writer-util :refer :all]
            [atom-finder.questions.operator-spacing :refer :all]
            [atom-finder.util :refer :all]
            [atom-finder.tree-diff :refer :all]
            ))

(deftest operator-spacing-test
  (testing "confusing-operator-spacing?"
    (let [cases [["a + b*c" false]
                 ["a*b + c"  false]
                 ["a+b * c"  true]
                 ["a||b && c"  true]
                 ["a&&b && c"  false]
                 ["cout << a<<b" false]
                 [" || t < std::numeric_limits<int32_t>::min()" false]
                 ]]

      (for [[node-str expected] cases]
        (let [node (parse-expr node-str)]
          (is (= (confusing-operator-spacing? node) expected)
            (str node-str " -> " expected)
            ))))
  )

  (testing "binary-op-spacing"
    (let [cases [[[" " " "] "a + b*c"]
                 [["" ""]   "a+b * c"]
                 [["" ""]   "a||b && c"]
                 [[" " " "] "a && b && c"]
                 [[" " ""]  "a <=b"]
                 [["" " "]  "a<= b"]
                 ]]

      (for [[expected node-str] cases]
        (is (= expected (binary-op-spacing (parse-expr node-str))))))
    )

  (testing "mixed-spacing?"
    (let [cases [["a + b*c"      true]
                 ["a*b + c"      true]
                 ["a+b*c"        false]
                 ["a||b && c"    true]
                 ["a && b && c"  false]
                 ["a->b && c"    false]
                 ["a.b && c"     false]
                 ]]

      (for [[node-str expected] cases]
        (is (= expected (mixed-spacing? (parse-expr node-str))))))
    )

  (testing "remove-all-parens"
    (let [cases [["a + (b)"     "a + b"]
                 ;["(a*b)"      "a * b"] ;; doesn't check the top node
                 ["1+(a*b)"     "1 + a * b"]
                 ["1 + (-(a))"  "1 + -a"]
                 ["(1) + (2)"   "1 + 2"]
                 ["(f)(x)"      "f(x)"]
                 ]]

      (for [[node-str expected] cases]
        (is (= expected (->> node-str parse-frag remove-all-parens write-tree)))))
    )
  )
