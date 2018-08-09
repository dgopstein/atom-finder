;; For every commit, which atoms were added, and which were removed

(ns atom-finder.commits-added-removed
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
   [clj-cdt.clj-cdt :refer :all]
   [clj-cdt.writer-util :refer :all]
   )
  (:import [org.eclipse.cdt.core.dom.ast IASTNode]
           [org.eclipse.jgit.revwalk RevCommit RevCommitList RevWalk]
           )
  )

(s/defn atom-map-to-seq
  [atoms :- {s/Keyword [IASTNode]}]
  (->> atoms (mapcat (fn [[k atoms]] (map #(array-map :type k :node %) atoms))) (sort-by (comp offset :node))))

(s/defn find-all-atoms-seq :- [{:type s/Keyword :node IASTNode}]
  [node :- IASTNode]
  (->> node find-all-atoms atom-map-to-seq))

(s/defn atom-map-diff :- (s/maybe {s/Keyword [{:type s/Keyword :node IASTNode}]})
  [before :- {s/Keyword [IASTNode]} after :- {s/Keyword [IASTNode]}]
   (->>
    (diff-by (comp write-node :node) (atom-map-to-seq before) (atom-map-to-seq after))
    (map (partial-right select-keys [:original :revised]))
    (apply merge-with concat)))

(s/defn atom-seq-diff
  [before :- [IASTNode] after :- [IASTNode]]
   (->>
    (diff-by write-node before after)
    (map (partial-right select-keys [:original :revised]))
    (apply merge-with concat)))

(defmulti author-name class)
(defmethod author-name String [rev-commit] nil)
(defmethod author-name RevCommit [rev-commit]
  (-> rev-commit .getAuthorIdent .getName))

(defmulti author-email class)
(defmethod author-email String [rev-commit] nil)
(defmethod author-email RevCommit [rev-commit]
  (-> rev-commit .getAuthorIdent .getEmailAddress))

(s/defn intersects-lines?
  [range-set node :- IASTNode]
  (.intersects range-set
               (com.google.common.collect.Range/closed
                (start-line node) (end-line node))))

(s/defn contained-by-lines?
  [range-set node :- IASTNode]
  (or (.contains range-set (start-line node))
      (.contains range-set (end-line node))))

(s/defn has-lines? [node]
  (and (start-line node) (end-line node)))

(s/defn added-removed-atoms
  "Ignore parts of the files not contained in the patch"
  [srcs]
  (let [patch-bounds (-<>> (or (:patch srcs) (:patch-str srcs)) (patch-file <> (:file srcs))
                           patch-change-bounds flatten1 (map change-bound-to-ranges))]
    (when (not (empty? patch-bounds)) ;; e.g. changes in binary files
      (let [
        [old-bounds new-bounds] (->> patch-bounds (map #(select-values % [:old :new]))
                                     transpose (map range-set-co))
        {atoms-removed :original atoms-added     :revised}
          (atom-map-diff
           (->> srcs :atoms-before (map-values (partial filter #(or (not (has-lines? %)) (contained-by-lines? old-bounds %)))))
           (->> srcs :atoms-after  (map-values (partial filter #(or (not (has-lines? %)) (contained-by-lines? new-bounds %))))))
        {non-atoms-removed :original non-atoms-added :revised}
          (atom-seq-diff
           (->> srcs :non-atoms-before (filter #(or (not (has-lines? %)) (contained-by-lines? old-bounds %))))
           (->> srcs :non-atoms-after  (filter #(or (not (has-lines? %)) (contained-by-lines? new-bounds %)))))]

    {
     :rev-str (:rev-str srcs)
     :file (:file srcs)
     :removed-atoms atoms-removed
     :added-atoms atoms-added
     :removed-non-atoms non-atoms-removed
     :added-non-atoms non-atoms-added
     :author-name  (->> srcs :rev-commit author-name)
     :author-email (->> srcs :rev-commit author-email)
     }
    ))))

(s/defn count-added-removed-atoms
  [atoms-added :- {s/Keyword s/Any}]
  (update-with atoms-added {[:removed-atoms]     (partial frequencies-by :type)
                            [:added-atoms]       (partial frequencies-by :type)
                            [:removed-non-atoms] count
                            [:added-non-atoms]   count}))

(s/defn added-removed-atoms-count [srcs :- {:patch s/Any :rev-commit s/Any s/Any s/Any}] (some-> srcs added-removed-atoms count-added-removed-atoms))
