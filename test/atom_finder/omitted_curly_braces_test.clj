(ns atom-finder.omitted-curly-braces-test
  (:require [clojure.test :refer :all]
            [schema.test]
            [atom-finder.util :refer :all]
            [atom-finder.classifier :refer :all]
            [atom-finder.constants :refer :all]
            ))

(use-fixtures :once schema.test/validate-schemas)

(deftest test-omitted-curly-braces-atom?
  (->> "curly-braces-in-macro.c"
       resource-path
       slurp
       parse-source
       ((default-finder omitted-curly-braces-atom?)))

  ;(->> "curly-braces-in-macro.c"
  ;     resource-path
  ;     slurp
  ;     parse-source
  ;     (get-in-tree [0 2 0 0 0])
  ;     (get-in-tree [1])
  ;     .getSyntax
  ;     )

  (testing "omitted-curly-braces-atom? finds all atoms in snippet study code"
    (test-atom-lines "omitted-curly-braces.c" "<true>" (default-finder omitted-curly-braces-atom?))))
