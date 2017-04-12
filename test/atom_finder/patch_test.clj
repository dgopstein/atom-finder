(ns atom-finder.patch-test
  (:require [clojure.test :refer :all]
            [atom-finder.util :refer :all]
            [atom-finder.constants :refer :all]
            [atom-finder.patch :refer :all]
            [atom-finder.results-util :refer :all]
            [clojure.pprint :refer :all]
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
              resource-path slurp parse-diff
              patch-correspondences correspondences-to-ranges)
         ))
