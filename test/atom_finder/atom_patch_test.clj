(ns atom-finder.atom-patch-test
  (:require [clojure.test :refer :all]
            [atom-finder.util :refer :all]
            [atom-finder.constants :refer :all]
            [atom-finder.patch :refer :all]
            [atom-finder.atom-patch :refer :all]
            [atom-finder.source-versions :refer :all]
            [atom-finder.classifier :refer :all]
            [clj-jgit.porcelain  :as gitp]
            [clojure.pprint :refer :all]
            ))

(deftest removed-lines-test
  (testing "Which lines were removed in this patch"
    (let [patch (->> "97574c57cf26ace9b8609575bbab66465924fef7_partial.patch" resource-path slurp)
          rl    (removed-lines patch)]
      
      (is (= rl {"gcc/ChangeLog" [16 26 27 28 29] "gcc/config/sparc/sparc.c" []}))
      )))

(when gcc-repo
  (deftest bugzilla-id-test
    (let [repo    gcc-repo
          results
          [["97574c57cf26ace9b8609575bbab66465924fef7" #{}]
           ["98103e4a9e8ae9e52751c9e96ec46e6095181b69" #{["fortran" 61420] ["fortran" 78013]}]
           ["b9db1ed4a901e9c0af7ac8cc5d4d933b5b9fd4b5" #{["libstdc++" 68838]}]
           ["bf8e44c9e49d658635b5a2ea4905333fa8845d1f" #{["debug" 77985]}] ]
          ]

      (doseq [[revstr expected] results]
        (testing (str "Which bugzilla id corresponds to commit " revstr)
          (is (= expected (bugzilla-ids (find-commit repo revstr))))
          ))))


  (deftest commit-file-atom-count-test
    (testing "See if file in commit contains an atoms"
      (let [repo    gcc-repo]
        (is (= 24 (commit-file-atom-count repo "3bb246b3c2d11eb3f45fab3b4893d46a47d5f931" "gcc/c-family/c-pretty-print.c" conditional-atom?)))
        (is (= 25 (commit-file-atom-count repo "1e0cfd0" "gcc/c-family/c-pretty-print.c" conditional-atom?)))
        )))

 (deftest apply-before-after-test
   (testing "Count the sizes of programs before and after a commit"
     (let [repo  ag-repo
           commit-hash "05be1eddca2bce1cb923afda2b6ab5e67faa248c"
           file-name "src/print.c"]
       (is (= [1892 2192] (apply-before-after repo commit-hash file-name count-nodes)))
     )))

 (deftest atom-removed-in-file?-test
   (testing "atom-removed-in-file?"
     (let [repo gcc-repo
           commit-hash "3bb246b3c2d11eb3f45fab3b4893d46a47d5f931"
           file-name "gcc/c-family/c-pretty-print.c"
           srcs (source-before-after repo commit-hash file-name)]
       (is (false?
           (atom-removed-in-file?
            (->> atom-lookup :logic-as-control-flow :finder)
            srcs)))
       (is (true?
           (atom-removed-in-file?
            (->> atom-lookup :conditional :finder)
            srcs)))
     )))
 )
