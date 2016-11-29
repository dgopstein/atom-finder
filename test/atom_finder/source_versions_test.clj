(ns atom-finder.source-versions-test
  (:require [clojure.test :refer :all]
            [atom-finder.util :refer :all]
            [atom-finder.patch :refer :all]
            [atom-finder.core :refer :all]
            ))


(deftest context-lines-test
  (testing "Parsing the @@ lines of a patch file"
    (is (= (context-lines "@@ -1,2 +3,4 @@") [[1 2 3 4]])
    (is (= (context-lines "@@ -1 +3 @@") [[1 1 3 1]]))
    )))

(deftest removed-lines-test
  (testing "Which lines were removed in this patch"
    (let [patch (->> "97574c57cf26ace9b8609575bbab66465924fef7_partial.patch" resource-path slurp)
          rl    (removed-lines patch)]
      
      (is (= rl {"gcc/ChangeLog" [[16 1] [26 4]] "gcc/config/sparc/sparc.c" []}))
      )))
