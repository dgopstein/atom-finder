(ns atom-finder.post-increment-test
  (:require [clojure.test :refer :all]
            [schema.test]
            [atom-finder.util :refer :all]
            [atom-finder.classifier :refer :all]
            ))

(use-fixtures :once schema.test/validate-schemas)

(deftest test-post-*crement-atom?
  (testing "post-*crement? finds all atoms in c file"
    (let [filepath   (resource-path "post-increment.c")
          expected   (true-lines filepath)
          lines  (->> filepath
                      tu
                      ((atom-finder.classifier/default-finder post-*crement-atom?))
                      (map loc)
                      (map :line))]

     (is (empty? (sort (sym-diff (set expected) (set lines))))
    )))

  (->>
((atom-finder.classifier/default-finder post-*crement-atom?) (parse-expr "y = (a++, b)"))
(map write-ast))

  (->>
   ;"(a, b++)"
   "a, b++"
   parse-expr
   post-*crement-atom?
   )
