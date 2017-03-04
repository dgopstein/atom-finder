(ns atom-finder.assignment-as-value-test
  (:require [clojure.test :refer :all]
            [schema.test]
            [atom-finder.util :refer :all]
            [atom-finder.classifier :refer :all]
            ))

(use-fixtures :once schema.test/validate-schemas)

(deftest test-assignment-as-value?
  (testing "assignment-as-value-atoms finds all atoms in c file"
    (let [filepath   (resource-path "assignment-as-value.c")
          expected   (true-lines filepath)
          lines  (->> filepath
                      tu
                      (:finder (:assignment-as-value atom-lookup))
                      (map loc)
                      (map :line))]

     ; (is (= expected lines))
    )))

(def tree (->>
            "assignment-as-value.c"
            resource-path
            tu
            ))
      (def f1 (get-in-tree [0 2 1] tree)) ; false
      (def t1 (get-in-tree [0 2 2 0] tree)) ; true
      (def t2 (get-in-tree [0 2 3 1] tree)) ; true
      (def t2 (get-in-tree [0 2 4] tree))   ; true
      (def f2 (get-in-tree [0 2 5 2] tree)) ; false
      (def t3 (get-in-tree [0 2 6] tree)) ; true

  (write-ast t1)
  (map write-ast (children t2))

  (write-ast f1)
  (typename f1)
 )

(->>
 "assignment-as-value.c"
 resource-path
 tu
 ;(get-in-tree [0 2 3]) ; false
 (get-in-tree [0 2 4]) ; false
 children
 (map typename)
 )
