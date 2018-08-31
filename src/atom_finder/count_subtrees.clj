(ns atom-finder.count-subtrees
  (:require [atom-finder.util :refer :all]
            [clojure.data.csv :as csv]
            [clojure.java.io  :as io]
            [clojure.string   :as string]
            )
  (:import [org.eclipse.cdt.core.dom.ast IASTExpression]
           [org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTIdExpression CPPASTName CPPASTLiteralExpression CPPASTPointer CPPASTSimpleDeclSpecifier CPPASTDeclarator CPPASTNullStatement CPPASTArrayModifier CPPASTDefaultStatement CPPASTProblem CPPASTProblemStatement CPPASTCompoundStatement CPPASTExpressionStatement
            ]
            ))

(defn astrminal
  "string that describes the tree structure with terminals"
  [node]
  (str (string/join "," (cons (typename node)
       (map astrminal (children node))))
       ")"))

(defn astr
  "string that describes the tree structure"
  [node]
  (str (string/join "," (cons (typename node)
       (map astr (filter (comp not leaf?) (children node)))))
       ")"))

(defn pastr
  "pretty string that describes the tree structure"
  ([node] (pastr 0 node))
  ([d node]
    (string/join (cons (str (typename node) "\n")
                       (map #(str (string/join (repeat d "    ")) (pastr (inc d) %))
                            (filter (comp not leaf?) (children node)))))
         ))

(defn pastr2
  "pretty string that describes the tree structure"
  ([node] (pastr 0 node))
  ([d node]
   (if (leaf? node)
     ""
     (string/join (cons (str (typename node) "\n")
                        (map #(str (string/join (repeat d "    ")) (pastr (inc d) %))
                             (children node)))))))

(defn count-nodes-of-height
  "count every similar tree of specified height"
  [d root]
  (->> root
       (filter-height d)
       (map astr)
       frequencies))

(defn count-all-expression-parents
  "Count node structures with expressions as their d'th height children"
  [d root]
  (->> root
       (filter-instance IASTExpression)
       (map (partial ancestor d))
       (distinct)
       (map typename)
       frequencies))

; CPPASTPointer only appears in declarations
(def trivial-types #{CPPASTIdExpression CPPASTName CPPASTLiteralExpression CPPASTPointer CPPASTSimpleDeclSpecifier CPPASTDeclarator CPPASTNullStatement CPPASTArrayModifier CPPASTDefaultStatement CPPASTProblem CPPASTProblemStatement})

(defn non-trivial?
  "Find AST nodes that aren't simply literals or identifiers (non-terminal)"
  [node]
  (not (trivial-types (class node))))

(def collapsible-types #{CPPASTExpressionStatement}) ; CPPASTCompoundStatement})

(defn collapse-types
  "For any node that's only a container for it's next-higher ancestor, return that ancestor instead"
  [node]
  (if (not (collapsible-types (class node)))
    node
    (recur (parent node))))

(defn non-trivial-expression-parents
  "find non-trivial (non-leaf) node structures with expressions as their d'th height children"
  [d root]
  (->> root
       (filter-instance IASTExpression)
       (filter non-trivial?)
       (map (partial ancestor d))
       (map collapse-types)
       ))


(defn count-non-trivial-expression-parents
  "Count non-trivial (non-leaf) node structures with expressions as their d'th height children"
  [d root]
  (->> root
       (non-trivial-expression-parents d)
       distinct
       (map typename)
       frequencies
       ))


(defn count-in-dir
  [f dirname]
  (reduce (partial merge-with +) (pmap-dir-c-files (comp f parse-file) dirname)))

(defn count-nodes-in-dir [dirname]
  (count-in-dir count-nodes dirname))

(defn count-nodes-of-height-in-dir [d dirname]
  (count-in-dir (partial count-nodes-of-height d) dirname))

(defn count-expression-parents-in-dir [d dirname]
  (count-in-dir (partial count-non-trivial-expression-parents d) dirname))

(defn count-nodes-in-dirs
  [dirs]
  (csv/write-csv *out*
    (apply concat (for [dir dirs]
      (let [name (.getName (io/file dir))]
        (map #(cons name %) (count-nodes-in-dir dir))
        )))))
