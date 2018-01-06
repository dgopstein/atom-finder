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
  (testing "atom-map-diff"
    (= (->>
        (atom-map-diff (->> "int x = z = y++" parse-frag root-ancestor find-all-atoms)
                       (->> "int x = z = w ? y : y++" parse-frag root-ancestor find-all-atoms))
        :revised (map #(->> % :node class .getSimpleName)))
       ;; one atom is the ?: itself, and the other is the parent of the y++, which moved out of the assignment
       ["CPPASTConditionalExpression" "CPPASTConditionalExpression"]))

  (testing "gcc commit regression"
    (let [rev-commit (find-rev-commit gcc-repo "3bb22d5fa5f279e90cff387b5db4644a620b5576")
          snprintf_lite-src
          (merge
          (before-after-data gcc-repo rev-commit "libstdc++-v3/src/c++11/snprintf_lite.cc")
           {:rev-commit rev-commit})]

      (->> snprintf_lite-src
           added-atoms
           :added-atoms
           (= {:operator-precedence 1, :pre-increment 1, :repurposed-variable 1})
           is)

      (->> snprintf_lite-src
           added-atoms-local
           :added-atoms
           (= {:operator-precedence 1, :pre-increment 1, :repurposed-variable 1})
           is)
      ))
  )

