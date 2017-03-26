(ns atom-finder.atom-patch-test
  (:require [clojure.test :refer :all]
            [atom-finder.util :refer :all]
            [atom-finder.constants :refer :all]
            [atom-finder.patch :refer :all]
            [atom-finder.atom-patch :refer :all]
            [atom-finder.source-versions :refer :all]
            [atom-finder.classifier :refer :all]
            [clj-jgit.porcelain  :as gitp]
            [clj-jgit.querying :as gitq]
            [clj-jgit.internal :as giti]
            [clojure.pprint :refer :all]
            ))

(deftest removed-lines-test
  (testing "Which lines were removed in this patch"
    (let [patch (->> "97574c57cf26ace9b8609575bbab66465924fef7_partial.patch" resource-path slurp)
          rl    (removed-lines patch)]

      (is (= rl {"gcc/ChangeLog" [16 26 27 28 29] "gcc/config/sparc/sparc.c" []}))
      )))

(when gcc-repo
 (deftest apply-before-after-test
   (testing "Count the sizes of programs before and after a commit"
     (let [repo  ag-repo
           rev-commit (find-rev-commit repo "05be1eddca2bce1cb923afda2b6ab5e67faa248c")
           file-name "src/print.c"]
       (is (= [1892 2192] (apply-before-after repo rev-commit file-name count-nodes)))
     )))

 (deftest atom-in-file-counts-test
   (testing "atom-in-file-counts"
     (let [repo gcc-repo
           rev-commit (find-rev-commit repo "3bb246b3c2d11eb3f45fab3b4893d46a47d5f931")
           file-name "gcc/c-family/c-pretty-print.c"
           srcs (source-before-after repo rev-commit file-name)]
       (is (= [28 28] (-> atom-lookup :logic-as-control-flow :finder (atom-in-file-counts srcs) vals)))
       (is (= [25 24] (-> atom-lookup :conditional :finder (atom-in-file-counts srcs) vals)))
     )))
 )
