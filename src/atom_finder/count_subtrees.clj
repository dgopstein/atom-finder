(ns atom-finder.count-subtrees
  (:require [atom-finder.util :refer :all]
            [clojure.data.csv :as csv]
            [clojure.java.io  :as io]
            [clojure.string   :as string]
            )
  (:import [org.eclipse.cdt.core.dom.ast gnu.cpp.GPPLanguage ASTVisitor IASTExpression]
            ))

(defn count-nodes
  "count every syntax node type in a tree"
  [root]
  (let [local-count {(typename root) 1}
        child-count (->> root children (map count-nodes))
        all-count   (conj child-count local-count)]

  (reduce (partial merge-with +) all-count)))

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

(defn count-nodes-of-depth
  "count every similar tree of specified depth"
  [d root]
  (->> root
       (filter-depth d)
       (map astr)
       frequencies))

(defn count-expression-parents
  "Count node structures with expressions as children"
  [root]
  (->> root
       (filter-instance-parent IASTExpression)
       ; (filter #(= (depth %) 3))
       ; (map astr)
       (map typename)
       frequencies))

(defn count-in-dir
  [f dirname]
  (reduce (partial merge-with +) (pmap-dir-nodes f dirname)))
  
(defn count-nodes-in-dir [dirname]
  (count-in-dir count-nodes dirname))

(defn count-nodes-of-depth-in-dir [d dirname]
  (count-in-dir (partial count-nodes-of-depth d) dirname))

(defn count-expression-parents-in-dir [dirname]
  (count-in-dir count-expression-parents dirname))
  
(defn count-nodes-in-dirs
  [dirs]
  (csv/write-csv *out*
    (apply concat (for [dir dirs]
      (let [name (.getName (io/file dir))]
        (map #(cons name %) (count-nodes-in-dir dir))
        )))))
