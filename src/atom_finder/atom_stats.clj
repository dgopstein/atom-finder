(ns atom-finder.atom-stats
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.constants :refer :all]
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

(defn comments-ba [srcs atom]
  {:comments-before (->> srcs :ast-before all-comments)
   :comments-after  (->> srcs :ast-after all-comments)})

(defn atom-stats [] {
   ;:atoms-ba                 atoms-ba
   :atom-counts-before-after ba-counts
   :source-size-before-after source-size-before-after
   :comments-before-after    comments-ba
   })
