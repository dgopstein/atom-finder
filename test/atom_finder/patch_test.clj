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
            ({:old [1 1] :new [2 13]}
             {:old [15 16] :new [27 28]}
             {:old [26 29] :new [38 40]})}
           {:file "/gcc/config/sparc/sparc.c"
            :ranges
            ({:old [650 650] :new [650 651]}
             {:old [869 869] :new [870 872]}
             {:old [2753 2753] :new [2756 2763]})})

         (->> "patch/97574c57cf26ace9b8609575bbab66465924fef7_partial.patch"
              parse-diff patch-correspondences)
         ))
    ))


