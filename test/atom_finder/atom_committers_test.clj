(ns atom-finder.atom-committers-test
  (:require [clojure.test :refer :all]
            [atom-finder.util :refer :all]
            [atom-finder.constants :refer :all]
            [atom-finder.patch :refer :all]
            [atom-finder.atom-patch :refer :all]
            [atom-finder.questions.atom-committers :refer :all]
            [atom-finder.classifier :refer :all]
            [clj-jgit.porcelain  :as gitp]
            [clj-jgit.querying :as gitq]
            [clj-jgit.internal :as giti]
            [clojure.pprint :refer :all]
            ))

(deftest atom-committers-test
  (testing "atom-diff"
    (= (->>
        (atom-diff (find-all-atoms (parse-frag "int x = z = y++")) (find-all-atoms (parse-frag "int x = z = w ? y : y++")))
        :revised (map #(->> % class .getSimpleName)))
       ;; one atom is the ?: itself, and the other is the parent of the y++, which moved out of the assignment
       ["CPPASTConditionalExpression" "CPPASTConditionalExpression"])))

