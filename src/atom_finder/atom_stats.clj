(ns atom-finder.atom-stats
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.classifier-util :refer :all]
   [atom-finder.constants :refer :all]
   [atom-finder.comment-change :refer :all]
   [clojure.pprint :refer [pprint]]
   [schema.core :as s]
   )
  )

(defn ba-counts
  [srcs atom]
  {:atom-count-before (->> srcs :atoms-before count)
   :atom-count-after  (->> srcs :atoms-after  count)})

(defn source-size-before-after [srcs atom]
  {:source-chars-before (-> srcs :source-before count)
   :source-chars-after  (-> srcs :source-after count)})

(defn ast-size [srcs atom]
  {:ast-size-before (-> srcs :ast-before flatten-tree count)
   :ast-size-after  (-> srcs :ast-after flatten-tree count)})

;(defn added-comments [srcs atom]
;  (let [cmnts-added (comments-added srcs)
;        atom-cmnts-added (atom-comments (:atoms-after srcs) cmnts-added)
;        n-cmnts-added (count cmnts-added)
;        n-atom-cmnts-added (count atom-cmnts-added)]
;  {:comments-added n-cmnts-added
;   :comments-added-near-atoms n-atom-cmnts-added
;   :comments-added-away-atoms (- n-cmnts-added n-atom-cmnts-added)}))

;(def srcs (atom-finder.atom-patch/before-after-data gcc-repo (atom-finder.source-versions/find-rev-commit gcc-repo "519d8cb612e9d641d133e4f65b4a48c3ef963f43") "gcc/lra-constraints.c"))
;(added-comments-context srcs (atom-finder.classifier/atom-lookup :omitted-curly-braces))

(defn added-comments-context [srcs atom]
  (let [cmnts-added (atom-finder.comment-change/comments-added srcs)
        {inner-cmnts-added true outer-cmnts-added false} (group-by in-function? (map offset-parent cmnts-added))
        {inner-atoms true outer-atoms false} (group-by in-function? (:atoms-after srcs))
        {inner-ast true outer-ast false} (group-by in-function? (flatten-tree (:ast-after srcs)))
        inner-atom-cmnts-added (atom-finder.comment-change/atom-comments inner-atoms cmnts-added)
        outer-atom-cmnts-added (atom-finder.comment-change/atom-comments outer-atoms cmnts-added)
        n-cmnts-added (count cmnts-added)
        n-inner-cmnts-added (count inner-cmnts-added)
        n-outer-cmnts-added (count outer-cmnts-added)
        n-inner-atom-cmnts-added (count inner-atom-cmnts-added)
        n-outer-atom-cmnts-added (count outer-atom-cmnts-added)]
  {:comments-added n-cmnts-added
   :inner-comments-added n-inner-cmnts-added
   :outer-comments-added n-outer-cmnts-added

   :comments-added-near-atoms (+ n-inner-atom-cmnts-added n-outer-atom-cmnts-added)
   :inner-comments-added-near-atoms n-inner-atom-cmnts-added
   :outer-comments-added-near-atoms n-outer-atom-cmnts-added

   :comments-added-away-atoms (- n-cmnts-added n-inner-atom-cmnts-added n-outer-atom-cmnts-added)
   :inner-comments-added-away-atoms (- n-inner-cmnts-added n-inner-atom-cmnts-added)
   :outer-comments-added-away-atoms (- n-outer-cmnts-added n-outer-atom-cmnts-added)

   :inner-atom-count (count inner-atoms)
   :outer-atom-count (count outer-atoms)
   :inner-ast-size
   :outer-ast-size
   }))

(defn atom-stats [] {
   :atom-counts-before-after ba-counts
   :source-size-before-after source-size-before-after
   :ast-size ast-size
   :added-comments added-comments-context
   })
