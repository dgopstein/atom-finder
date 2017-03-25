(ns atom-finder.source-versions-test
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

(when gcc-repo
  (deftest bugzilla-id-test
    (let [repo    gcc-repo
          results
          [["97574c57cf26ace9b8609575bbab66465924fef7" #{}]
           ["98103e4a9e8ae9e52751c9e96ec46e6095181b69" #{{:branch "fortran" :bug-id 61420} {:branch "fortran" :bug-id 78013}}]
           ["b9db1ed4a901e9c0af7ac8cc5d4d933b5b9fd4b5" #{{:branch "libstdc++" :bug-id 68838}}]
           ["bf8e44c9e49d658635b5a2ea4905333fa8845d1f" #{{:branch "debug" :bug-id 77985}}]
           ["6d34050702a3fc983620fd5f2ae89cff243b6bbd" #{{:branch "ipa" :bug-id 78721}}]
           ]]

      (doseq [[revstr expected] results]
        (testing (str "Which bugzilla id corresponds to commit " revstr)
          (is (= expected (bugzilla-ids (find-commit repo revstr))))
          ))))

)
