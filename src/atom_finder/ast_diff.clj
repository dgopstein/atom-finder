(ns atom-finder.ast-diff
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.constants :refer :all]
   [clojure.pprint :refer [pprint]]
   [schema.core :as s]
   )
  (:import
   [org.eclipse.cdt.core.dom.ast IASTNode]
   [distance APTED]
   [costmodel CostModel]
   [node Node]
   ))

(def ASTUnitCostModel
  (proxy [CostModel] []
    (del [node] 1.0)
    (ins [node] 1.0)
    (ren [a b] (if (= (->> a .getNodeData .toString) (->> b .getNodeData .toString)) 0.0 1.0))
    ))

(defn ASTDiffNode
  ([node] (ASTDiffNode node []))
  ([node children]
   (let [node-obj
         (proxy [Node] [node]
           (toString [] (.toString (proxy-super getNodeData))))]

     (doseq [child children] (.addChild node-obj child))

     node-obj)))

(def apted #(APTED. ASTUnitCostModel))

(defn postorder [node]
  (concat (->> node .getChildren (mapcat postorder)) [node]))

(defn post-ids [node]
  (->> (postorder node)
       (map-indexed #(vector (inc %1) %2))
       (into {})
       ))

(postorder (ASTDiffNode "a" [(ASTDiffNode "b") (ASTDiffNode "c")]))
(->> ;(ASTDiffNode "a" [(ASTDiffNode "b") (ASTDiffNode "c")])
     (ASTDiffNode "a" [(ASTDiffNode "b") (ASTDiffNode "c") (ASTDiffNode "d")])
     post-ids
     (map-values (memfn getNodeData))
     )


(def ast-before (ASTDiffNode "a" [(ASTDiffNode "b") (ASTDiffNode "c" [(ASTDiffNode "f")])]))
(def ast-after (ASTDiffNode "a" [(ASTDiffNode "b") (ASTDiffNode "c") (ASTDiffNode "d") (ASTDiffNode "e")]))

(defn added-nodes [apted _ast-before ast-after]
  (let [after-post-ids (post-ids ast-after)]
    (->> apted
         .computeEditMapping
         (filter (comp zero? first))
         (map last)
         (map after-post-ids)
         )
    ))

(defn removed-nodes [apted ast-before _ast-after]
  (let [begin-post-ids (post-ids ast-before)]
    (->> apted
         .computeEditMapping
         (filter (comp zero? last))
         (map first)
         (map begin-post-ids)
         )
    ))


(def ast-a (->> "gcc_cp_pt.c_d430756d2dbcc396347bd60d205ed987716b5ae8" parse-resource))
(def ast-b (->> "gcc_cp_pt.c_92884c107e041201b33c5d4196fe756c716e8a0c" parse-resource))
(def ast-a (->> "diff_before.c" parse-resource))
(def ast-b (->> "diff_after.c" parse-resource))

(s/defn diff-nodify :- Node [node :- IASTNode]
  (ASTDiffNode node (map diff-nodify (children node))))

(s/defn tree-diff [ast-a :- IASTNode ast-b :- IASTNode]
  (let [apted (APTED. ASTUnitCostModel)
        a (diff-nodify ast-a)
        b (diff-nodify ast-b)]
    (.computeEditDistance apted a b)

    {:nodes-added   (map (memfn getNodeData) (added-nodes apted a b))
     :nodes-removed (map (memfn getNodeData) (removed-nodes apted a b))}
    ))

(->> (tree-diff ast-a ast-b)
     (map-values #(map write-ast %))
     pprint
     )

(.computeEditDistance apted ast-before ast-after)
(added-nodes apted ast-before ast-after)
(removed-nodes apted ast-before ast-after)

(->> (.computeEditMapping apted)
     (map pprint))
