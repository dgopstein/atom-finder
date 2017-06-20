(ns atom-finder.patch-test
  (:require [clojure.test :refer :all]
            [atom-finder.util :refer :all]
            [atom-finder.constants :refer :all]
            [atom-finder.patch :refer :all]
            [atom-finder.tree-diff.difflib :as difflib]
            [atom-finder.zutubi :as zutubi]
            [clojure.pprint :refer :all]
            ))

(deftest patch-libs-test
  (testing "Both patch parsers get the same results"
    (let [patch-str (->> "patch/97574c57cf26ace9b8609575bbab66465924fef7_partial.patch" slurp-resource)
          z-patch (->> patch-str zutubi/parse-diff)
          d-patch (->> patch-str difflib/parse-diff)]
      (is (= (->> z-patch (mapcat deltas) (take 1) (map old-offset))
             (->> d-patch (mapcat deltas) (take 1) (map old-offset))))
      (is (thrown? RuntimeException (->> d-patch (mapcat deltas) (drop 1) (map old-offset) dorun))) ; this parser doesn't work on text after @@'s
      nil
      )
    ))

(deftest hunk-line-ranges-test
  (testing "Which lines were added and removed in a hunk"
    (is (=
         '({:file "gcc/ChangeLog"
            :ranges
            ({:old [1 1] :new [1 13]}
             {:old [16 17] :new [28 29]}
             {:old [26 30] :new [38 41]})}
           {:file "gcc/config/sparc/sparc.c"
            :ranges
            ({:old [651 651] :new [651 652]}
             {:old [869 869] :new [870 873]}
             {:old [2752 2752] :new [2756 2764]})})

         (->> "patch/97574c57cf26ace9b8609575bbab66465924fef7_partial.patch"
              slurp-resource zutubi/parse-diff
              patch-line-correspondences correspondences-to-ranges)
         ))
    ))
