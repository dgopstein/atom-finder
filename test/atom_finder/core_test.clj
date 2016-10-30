(ns atom-finder.core-test
  (:require [clojure.test :refer :all]
            [atom-finder.util :refer :all]
            [atom-finder.find-atom :refer :all]
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
       (is (instance? org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTTranslationUnit (translation-unit (resource-path filename))) (format "Parsing file: %s" filename))
    ))))

(deftest macros-in-context-test
  (testing "Macro entirely in context"
    (let [filename (resource-path "macro-in-expression.c")
          atoms (macros-in-contexts (translation-unit filename))]

      (is (= (map :line atoms) '(5 8 11)))
      ))

  (testing "Macro starts in context"
    (let [filename (resource-path "if-starts-in-expression.c")
          atoms (macros-in-contexts (translation-unit filename))]

      (is (= (map :line atoms) '(9 11 14 16 18 23))) ; technically 25 should be here too because otherwise it's dependent on which if branch is evaluated
      ))


  )
