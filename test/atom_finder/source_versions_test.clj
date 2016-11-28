(ns atom-finder.source-versions-test
  (:require [clojure.test :refer :all]
            [atom-finder.util :refer :all]
            [atom-finder.patch :refer :all]
            [atom-finder.core :refer :all]
            ))


(deftest context-lines
  (testing "Parsing the @@ lines of a patch file"
    (is (= (context-lines "@@ -1,2 +3,4 @@") [[1 2 3 4]])
    (is (= (context-lines "@@ -1 +3 @@") [[1 1 3 1]]))
    ))
