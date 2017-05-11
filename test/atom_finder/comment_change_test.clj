(ns atom-finder.comment-change-test
  (:require [clojure.test :refer :all]
            [schema.test]
            [atom-finder.util :refer :all]
            [atom-finder.test-util :refer :all]
            [atom-finder.classifier :refer :all]
            [atom-finder.classifier-util :refer :all]
            [atom-finder.comment-change :refer :all]
            ))

(use-fixtures :once schema.test/validate-schemas)

(deftest test-changed-comments
  (testing "comments are associated with their nearby atoms"
    (test-atom-lines "atom-comments.c" "<true>"
                     #(mapcat last (atom-finder-comments (:post-increment atom-lookup) %))))

  (testing "changes in comments are detected near atoms"
    (let [srcs
          {:ast-before (->> "comment-change-before.c" parse-resource)
           :ast-after (->> "comment-change-after.c" parse-resource)
           :atoms-before (->> "comment-change-before.c" parse-resource ((->> :post-increment atom-finder.classifier/atom-lookup :finder)))
           :atoms-after (->> "comment-change-after.c" parse-resource ((->> :post-increment atom-finder.classifier/atom-lookup :finder)))
           }]
      (is (= ["// added new atom-adjacent line"]
             (map (comp str last last)
                  (atom-comments-added srcs (atom-lookup :post-increment)))))
      )))
