(ns atom-finder.operator-spacing-test
  (:require [clojure.test :refer :all]
            [clj-cdt.clj-cdt :refer :all]
            [clj-cdt.writer-util :refer :all]
            [atom-finder.questions.operator-spacing :refer :all]
            [atom-finder.util :refer :all]
            [atom-finder.tree-diff :refer :all]
            ))

(deftest operator-spacing-test
  (testing "parse-by-spacing"
    (let [cases [["(a+b*c) + (d*3)" "a+b*c + d*3"]
                  ;["a*b+c * b+c" "(a*b+c) * (b+c)"] FAIL
                 ["a::b" "a::b"]
                 ["a->b" "a->b"]
                 ["1 + f(x)" "1+f(x)"]
                  ]]

      (for [[expected node-str] cases]
        (is (tree=by write-tree
                     (parse-by-spacing node-str)
                     (parse-expr expected))
            (str node-str " -> " expected)
            ))))

  (testing "confusing-operator-spacing?"
    (let [cases [["a + b*c" false]
                 ["a*b + c"  false]
                 ["a+b * c"  true]
                 ["a||b && c"  true]
                 ["a&&b && c"  false]
                 ["a->b && c"  false]
                 ["a.b && c"  false]
                 ["a::b<c>" false]
                 ["a::b::c" false]
                 ["y = a::b<c>(d)" false]
                 ["a->b" false]
                 ["(a)b * c" false]
                 ["(a)b.c * d" false]
                 ["a + sizeof(b)" false]
                 ["a + -b" false]
                 ["a + sizeof(struct b)" false]
                 ["a->b.c->d == e->f" false]
                 ["1+f(x)" false]
                 ["1+sizeof(x)*2" false]
                 ;;["a . b&&c"  true] ;; the rearrangement can't compile
                 ]]

      (for [[node-str expected] cases]
        (let [node (parse-expr node-str)]
          (is (= (confusing-operator-spacing? node) expected)
            (str node-str " -> " expected)
            ))))
  )

  (testing "mixed-spacing?"
    (let [cases [["a + b*c"      true]
                 ["a*b + c"      true]
                 ["a+b*c"        false]
                 ["a||b && c"    true]
                 ["a && b && c"  false]
                 ["a->b && c"    true]
                 ["a.b && c"     true]
                 ]]

      (for [[node-str expected] cases]
        (is (= expected (mixed-spacing? node-str)))))
    )

  (testing "binary-expr?"
    (let [cases [["a + b"     true]
                 ["a*b"       true]
                 ["-a"        false]
                 ["1"          false]
                 ["if (x) y;"  false]
                 ]]

      (for [[node-str expected] cases]
        (is (= expected (->> node-str parse-frag binary-expr?)))))
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
