(ns atom-finder.source-versions-test
  (:require [clojure.test :refer :all]
            [atom-finder.util :refer :all]
            [atom-finder.patch :refer :all]
            [atom-finder.source-versions :refer :all]
            [clj-jgit.porcelain  :as gitp]
            ))


(deftest removed-lines-test
  (testing "Which lines were removed in this patch"
    (let [patch (->> "97574c57cf26ace9b8609575bbab66465924fef7_partial.patch" resource-path slurp)
          rl    (removed-lines patch)]
      
      (is (= rl {"gcc/ChangeLog" [16 26 27 28 29] "gcc/config/sparc/sparc.c" []}))
      )))

(deftest bugzilla-id-test
  (let [repo    (gitp/load-repo (expand-home "~/opt/src/gcc"))
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
