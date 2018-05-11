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

    (->> "1 + 2" parse-frag seq-tree? not is)
    (->> "1 + 2" parse-frag seq-tree seq-tree? is)

    (->> "#include \"/Users/dgopstein/nyu/confusion/atom-finder/src/test/resources/atoms-removed-after.c\"\nint a;"
         parse-source
         (filter-seq-tree (complement from-include?))
         (get-in-tree [1])
         write-tree
         (= "int a;\n") is)
    ))

