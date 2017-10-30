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

(s/defn atom-map-to-seq
  [atoms :- {s/Keyword [IASTNode]}]
  (->> atoms (mapcat (fn [[k atoms]] (map #(array-map :type k :node %) atoms))) (sort-by (comp offset :node))))

(s/defn find-all-atoms-seq :- [{:type s/Keyword :node IASTNode}]
  [node :- IASTNode]
  (->> node find-all-atoms atom-map-to-seq))

(s/defn atom-diff
  [before :- {s/Keyword [IASTNode]} after :- {s/Keyword [IASTNode]}]
   (->>
    (diff-by (comp write-node-valueless :node) (atom-map-to-seq before) (atom-map-to-seq after))
    (map (partial-right select-keys [:original :revised]))
    (apply merge-with concat)))

(s/defn added-atoms
  [srcs]
  (let [{_ :original new-atoms :revised} (atom-diff (:atoms-before srcs) (:atoms-after srcs))]
    (for [{type :type new-atom :node} new-atoms]
      {
       :rev-str (:rev-str srcs)
       :file (:file srcs)
       :type type
       :node new-atom
       :start-line (start-line new-atom)
       :author (->> srcs :rev-commit .getAuthorIdent .toExternalString)
       })))

'((->> "12f7900694b31d218f837b477c35777d88d736f5"
     (commits-from gcc-repo)
     (take 20)
     (mapcat :srcs)
     (map added-atoms)
     (remove empty?)
     pprint
     time-mins
     ))
