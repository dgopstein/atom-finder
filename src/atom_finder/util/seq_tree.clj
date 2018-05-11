;; Represent an AST's tree-structure using only clojure sequences

(in-ns 'atom-finder.util)

(require '[clj-cdt.writer-util :refer [write-tree]])
(defmethod write-tree clojure.lang.ISeq [node] (->> node first write-tree))
(defmethod children clojure.lang.ISeq [node] (rest node))

(s/defn seq-tree? [node]
  (and (seqable? node)
       (or (instance? IASTNode (first node))
           (empty? (first node)))))

(s/defn seq-tree
  "A tree of nested lists that represents the AST"
  [node]
  (if (seq-tree? node)
    node
    (cons node (map seq-tree (children node)))))

(s/defn prune-seq-tree
  "Form new leaves at a given predicate"
  [f any-tree]
  (let [[x & xs] (seq-tree any-tree)]
    (cons x
          (->> xs
               (map #(if (f (first %1)) '() %1))
               (map #(if (= '() %1) '() (prune-seq-tree f %1)))))))

(s/defn filter-seq-tree
  "Form new leaves at a given predicate"
  [f any-tree]
  (let [[x & xs] (seq-tree any-tree)]
    (when (seqable? x) x)
    (when (and x (f x))
      (cons x
          (map (partial filter-seq-tree f) xs)))))
