;; What is the distribution of people who commit atoms?

(ns atom-finder.questions.atom-committers
  (:require
   [atom-finder.atoms-in-dir :refer :all]
   [atom-finder.atom-patch :refer :all]
   [atom-finder.classifier :refer :all]
   [atom-finder.constants :refer :all]
   [atom-finder.tree-diff.difflib :refer [diff-by diff-trees]]
   [atom-finder.patch :refer :all]
   [atom-finder.util :refer :all]
   [atom-finder.questions.edit-lines :refer :all]
   [clojure.pprint :refer :all]
   [clojure.string :as string]
   [swiss.arrows :refer :all]
   [schema.core :as s]
   )
  (:import [org.eclipse.cdt.core.dom.ast IASTNode])
  )

(def repo-heads
  [{gcc-repo "12f7900694b31d218f837b477c35777d88d736f5"}])

(s/defn add-change?
  [change-bounds :- {:old [s/Int s/Int] :new [s/Int s/Int]}]
  (apply = (:old change-bounds)))

(s/defn find-all-atoms-seq :- [IASTNode]
  [node :- IASTNode]
  (->> node find-all-atoms vals (remove empty?) flatten (sort-by offset)))

(defn added-atoms
  [a b]
  (->>
   (diff-by write-node-valueless (find-all-atoms-seq a) (find-all-atoms-seq b))
   (map (partial-right select-keys [:original :revised]))
   (apply merge-with concat)))

'((->> "12f7900694b31d218f837b477c35777d88d736f5"
     (commits-from gcc-repo)
     first
     :srcs
     first
     added-lines
     ))
