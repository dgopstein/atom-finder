(ns atom-finder.atom-stats
  (:require
   [atom-finder.util :refer :all]
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

(defn added-comments [srcs atom]
  (let [cmnts-added (comments-added srcs)
        atom-cmnts-added (atom-comments (:atoms-after srcs) cmnts-added)
        n-cmnts-added (count cmnts-added)
        n-atom-cmnts-added (count atom-cmnts-added)]
  {:comments-added n-cmnts-added
   :comments-added-near-atoms n-atom-cmnts-added
   :comments-added-away-atoms (- n-cmnts-added n-atom-cmnts-added)}))

(defn atom-stats [] {
   :atom-counts-before-after ba-counts
   :source-size-before-after source-size-before-after
   :ast-size ast-size
   :added-comments added-comments
   })
