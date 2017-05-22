(ns atom-finder.core-test
  (:require [clojure.test :refer :all]
            [atom-finder.util.util :refer :all]
            [atom-finder.classifier :refer :all]
            [atom-finder.core :refer :all]
            ))


(deftest hard-compilation
  (testing "Files that are difficult to compile"
    (let [files (%w
                 if-starts-in-expression.c
                 limits-declparen.c
                 macro-in-expression.c
                 typedef_union_struct.c
                 union.c
                 wdate-time.c)]
      (doseq [filename files]
       (is (instance? org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTTranslationUnit (parse-resource filename)) (format "Parsing file: %s" filename))
    ))))

