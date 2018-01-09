(ns atom-finder.commits-added-removed-test
  (:require [clojure.test :refer :all]
            [atom-finder.util :refer :all]
            [atom-finder.constants :refer :all]
            [atom-finder.patch :refer :all]
            [atom-finder.atom-patch :refer :all]
            [atom-finder.commits-added-removed :refer :all]
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

  (when gcc-repo
    (testing "gcc commit regression"
      (let [rev-commit (find-rev-commit gcc-repo "3bb22d5fa5f279e90cff387b5db4644a620b5576")
            snprintf_lite-src
            (merge
             (before-after-data gcc-repo rev-commit "libstdc++-v3/src/c++11/snprintf_lite.cc")
             {:rev-commit rev-commit})
            atoms-added (added-atoms snprintf_lite-src)]

        (->> atoms-added
             count-added-atoms
             :added-atoms
             (= {:operator-precedence 1, :pre-increment 1, :repurposed-variable 1})
             is)
        )

      (let [rev-commit (find-rev-commit gcc-repo "d430756d2dbcc396347bd60d205ed987716b5ae8")
            cp_pt-src (merge (before-after-data gcc-repo rev-commit "gcc/cp/pt.c")
                             {:rev-commit rev-commit})

            ;; this function used to take ~6mins, so fail it if it regresses over 20 seconds
            added-atoms-res (->> cp_pt-src added-atoms (with-timeout 20))
            ]


              (->> added-atoms-res :added-atoms first :node write-tree
                   (= "if (canonical_template_parms->length() <= (unsigned )idx)\n    vec_safe_grow_cleared(canonical_template_parms, idx + 1);\n")
                   is)

              (->> added-atoms-res :added-non-atoms (map write-node)
                   (= ["vec_safe_grow_cleared" "+" "idx" "1" ";" "=" "()" "<IdExpression>"
                       "TEMPLATE_PARM_PARAMETER_PACK" "<IdExpression>" "newidx" "()"
                       "<IdExpression>" "TEMPLATE_PARM_PARAMETER_PACK" "<IdExpression>" "oldidx"])
                   is)
      )))
  )
