(ns atom-finder.atom-patch-test
  (:require [clojure.test :refer :all]
            [atom-finder.util :refer :all]
            [atom-finder.constants :refer :all]
            [atom-finder.patch :refer :all]
            [atom-finder.atom-patch :refer :all]
            [atom-finder.atom-stats :refer :all]
            [atom-finder.source-versions :refer :all]
            [atom-finder.classifier :refer :all]
            [atom-finder.zutubi :as zutubi]
            [clj-jgit.porcelain  :as gitp]
            [clj-jgit.querying :as gitq]
            [clj-jgit.internal :as giti]
            [clojure.pprint :refer :all]
            ))

(deftest removed-lines-test
  (testing "Which lines were removed in this patch"
    (let [patch (->> "patch/97574c57cf26ace9b8609575bbab66465924fef7_partial.patch"
                     resource-path slurp)
          rl    (removed-lines patch)]

      (is (= rl {"gcc/ChangeLog" [16 26 27 28 29] "gcc/config/sparc/sparc.c" []}))
      )))

(deftest flatten-res-test
  (testing "Can data be flattened"

    (is (= {:a 1 :c 2 :d 3}
         (flatten-map {:a 1 :b {:c 2 :d 3}})))

    (is
     (= '({:atom :preprocessor-in-statement :count-before 0 :count-after 0})
        (flatten-res {:atom :preprocessor-in-statement,:stats {:atom-counts-before-after {:count-before 0,:count-after 0}}})))

    (is (= '({:revstr "123abc" :atom :preprocessor-in-statement} {:revstr "123abc" :atom :logic-as-control-flow})
     (flatten-res '{:revstr "123abc" :atoms ({:atom :preprocessor-in-statement} {:atom :logic-as-control-flow})})))

    (is (= '({:revstr "1a" :file "a.c" :atom :a}
             {:revstr "1a" :file "a.c" :atom :b}
             {:revstr "1a" :file "b.c" :atom :a}
             {:revstr "1a" :file "b.c" :atom :b})
           (flatten-res '({:revstr "1a" :files
                           ({:file "a.c" :atoms ({:atom :a} {:atom :b})}
                            {:file "b.c" :atoms ({:atom :a} {:atom :b})})}))))
    ))

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
           srcs (before-after-data repo rev-commit file-name)
           logic-as-control-flow (->> atom-lookup :logic-as-control-flow)
           conditional (->> atom-lookup :conditional)
           ]
       (is (= {:atom-count-before 28 :atom-count-after 28}
              (ba-counts (atom-specific-srcs srcs logic-as-control-flow) logic-as-control-flow)))
       (is (= {:atom-count-before 25 :atom-count-after 24}
              (ba-counts (atom-specific-srcs srcs conditional) conditional)))
     )))
 )

(deftest line-range-test
  (testing "utilities for line ranges"
    (let [cases [[[1 1] [1 2] false]
                 [[1 2] [1 1] false]
                 [[1 1] [1 1] false]
                 [[1 1] [0 2] true]
                 [[0 2] [1 1] true]
                 [[1 3] [2 3] true]
                 [[1 3] [2 4] true]
                 [[1 2] [3 4] false]
                 [[1 2] [2 4] false]
                 ]]
      (for [[a b expected] cases]
        (is (= expected (intersects? a b)) (str a ", " b " -> " expected))))

    (is (= true (multi-intersects? [[1 3] [8 10]] [[4 5] [9 10]])))
    (is (= false (multi-intersects? [[1 3] [8 10]] [[4 5] [19 20]])))
    ))
