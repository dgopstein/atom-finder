(ns atom-finder.atom-stats
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.constants :refer :all]
   [clojure.pprint :refer [pprint]]
   [schema.core :as s]
   )
  (:import
   [atom_finder.classifier Atom]
   [org.eclipse.jgit.lib ObjectReader Repository]
   [org.eclipse.jgit.api Git]
   [org.eclipse.jgit.revwalk RevCommit]
   [org.eclipse.jgit.treewalk TreeWalk filter.PathFilter]
   [org.eclipse.cdt.core.dom.ast IASTTranslationUnit IASTNode]
   [org.eclipse.cdt.internal.core.dom.parser ASTNode]
   [distance APTED]
   [costmodel CostModel]
   [node Node]
   )
  )

(s/defn ba-counts
  [srcs atom]
  {:atom-count-before (->> srcs :ast-before ((:finder atom)) count)
   :atom-count-after  (->> srcs :ast-after  ((:finder atom)) count)})

(defn source-size-before-after [srcs atom]
  {:source-chars-before (-> srcs :source-before count)
   :source-chars-after  (-> srcs :source-after count)})

(defn atom-stats [] {
   :atom-counts-before-after ba-counts
   :source-size-before-after source-size-before-after
   })


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

apted = new APTED<>(new StringUnitCostModel())
(def apted (APTED. ASTUnitCostModel))

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

(.computeEditDistance apted ast-before ast-after)
(added-nodes apted ast-before ast-after)
(removed-nodes apted ast-before ast-after)

(->> (.computeEditMapping apted)
     (map pprint))
