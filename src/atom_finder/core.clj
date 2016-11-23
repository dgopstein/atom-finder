(ns atom-finder.core
  (:require [atom-finder.classifier :refer :all]
            [atom-finder.count-subtrees :refer :all]
            [atom-finder.util :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as string]
            )
  (:import [org.eclipse.cdt.core.dom.ast gnu.cpp.GPPLanguage ASTVisitor IASTExpression]
           [org.eclipse.cdt.core.parser DefaultLogService FileContent IncludeFileContentProvider ScannerInfo]
           [org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTTranslationUnit]
           [org.eclipse.cdt.internal.core.dom.rewrite.astwriter ASTWriter]))

(defn -main
  [& args]

  (def root (tu (resource-path "logic-as-control-flow.c")))
  (def big-root (tu (expand-home "~/opt/src/github-top-c/php-src/ext/sqlite3/libsqlite/sqlite3.c")))

  (time (count (flatten (pmap-dir-nodes logic-as-control-flow-atoms (expand-home "~/opt/src/github-top-c")))))

  (->> root logic-as-control-flow-atoms (map loc))

  (->> big-root (filter-tree #(and (non-trivial? %1) (leaf? %1))) (map typename) frequencies (sort-by last) pprint)
  (->> big-root (filter-tree #(and (non-trivial? %1) (leaf? %1) (= (typename %1) "DefaultStatement"))) (take 20) (map write) pprint)
  (->> big-root (filter-tree #(and (non-trivial? %1) (leaf? %1) (= (typename %1) "ArrayModifier"))) (take 20) (map loc) pprint)
  (->> big-root (filter-tree #(and (non-trivial? %1) (leaf? %1) (= (typename %1) "ArrayModifier"))) (take 20) (map parent) (map write) pprint)
  (->> big-root (filter-tree #(and (non-trivial? %1) (leaf? %1) (= (typename %1) "Declarator"))) (take 20) (map parent) (map write))
  (->> big-root (filter-type "ArraySubscriptExpression") (filter #(non-trivial? (nth (children %) 1))) (take 100) (map write) distinct pprint)

  (->> big-root (filter-type "BinaryExpression") first collapse-types)
  (->> big-root (filter-type "CompoundStatement") first collapse-types)

  (->> big-root (filter-tree #(= (typename %) "IdExpression")) (mapcat children) (map typename) frequencies)

  (time (pprint (reverse (sort-by last (count-expression-parents-in-dir 1 (expand-home "~/opt/src/github-top-c/the_silver_searcher"))))))
  (time (pprint (reverse (sort-by last (count-expression-parents-in-dir 1 (expand-home "~/opt/src/github-top-c"))))))
  )
