(ns quark.tree-tokenizer-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [atom-finder.constants :refer :all]
            [atom-finder.util :refer :all]
            [clj-cdt.clj-cdt :refer :all]
            [clj-cdt.writer-util :refer :all]
            [quark.tree-tokenizer :refer :all]
            ))

(when (-> system-include-paths empty? not)
  (deftest system-include-test
    (-> "#include <algorithm>"
        (parse-source {:include-dirs system-include-paths})
        write-tree empty? not is)
    ))

