(ns atom-finder.superfluous-parens-test
  (:require [clojure.test :refer :all]
            [clj-cdt.clj-cdt :refer :all]
            [atom-finder.util :refer :all]
            [atom-finder.questions.superfluous-parens :refer :all]
            ))

(deftest superfluous-parens?-test
  (testing "superfluous-parens?"

    (let [cases [[true  "1 + (2 * 3)" 1]
                 [false "(1 + 2) * 3" 0]
                 [false "f((x, y))"   1]
                 [true  "f((x), y))"  1]
                 ]]

      (for [case cases
            :let [[expected src idx] case]]

        (->> src parse-expr (get-in-tree [idx]) superfluous-parens? (= expected) is)
        ))
    )
  )

