(ns quark.tree-tokenizer-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [swiss.arrows :refer :all]
            [atom-finder.constants :refer :all]
            [atom-finder.util :refer :all]
            [clj-cdt.clj-cdt :refer :all]
            [clj-cdt.writer-util :refer :all]
            [quark.tree-tokenizer :refer :all]
            ))

(deftest serialization-test
  (testing "to-edn"
  (->> "x" parse-frag expr-typename (= "problem-type") is)
  (->> "f(x)" parse-frag expr-typename (= "problem-binding") is)
  (->> "1" parse-frag tree-to-edn (= ["LiteralExpression" "int"]))
  (->> "{int x; x;}" parse-frag tree-to-edn
       (= '(["CompoundStatement" nil "{}"]
           (["DeclarationStatement" nil "<DeclarationStatement>"]
            (["SimpleDeclaration" nil ";"] ["SimpleDeclSpecifier" nil "int"]
             (["Declarator" nil "<Declarator>"] ["Name" "x"])))
           (["ExpressionStatement" nil ";"]
            (["IdExpression" "int" "<IdExpression>"] ["Name" "x"])))
          ) is)
  (->> "\"abc\" + 2" parse-frag tree-to-edn
       (= '(["BinaryExpression" "const char *" "+"]
            ["LiteralExpression" "const char [4]" "\"abc\""]
            ["LiteralExpression" "int" "2"])) is)
  (->> "if(1) 'b';" parse-frag tree-to-edn
       (= '(["IfStatement" nil "if()"] ["LiteralExpression" "int" "1"]
            (["ExpressionStatement" nil ";"] ["LiteralExpression" "char" "'b'"]))) is)

  ))

(when (-> system-include-paths empty? not)
  (deftest system-include-test
    (-> "#include <algorithm>"
        (parse-source {:include-dirs system-include-paths})
        write-tree empty? not is)

    (->> "1 + 2" parse-frag seq-tree? not is)
    (->> "1 + 2" parse-frag seq-tree seq-tree? is)

    (-<>> "#include \"/Users/dgopstein/nyu/confusion/atom-finder/src/test/resources/atoms-removed-after.c\"\nint a;"
         (parse-source <> {:resolve-includes true})
         (filter-seq-tree (complement from-include?))
         (get-in-tree [1])
         write-tree
         (= "int a;\n") is)
    ))

