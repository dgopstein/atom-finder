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
  (:import [org.eclipse.cdt.core.dom.ast IASTNode]
           [org.eclipse.jgit.revwalk RevCommit RevCommitList RevWalk]
           )
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

(s/defn atom-map-diff :- (s/maybe {s/Keyword [{:type s/Keyword :node IASTNode}]})
  [before :- {s/Keyword [IASTNode]} after :- {s/Keyword [IASTNode]}]
   (->>
    (diff-by (comp write-node-valueless :node) (atom-map-to-seq before) (atom-map-to-seq after))
    (map (partial-right select-keys [:original :revised]))
    (apply merge-with concat)))

(s/defn atom-seq-diff
  [before :- [IASTNode] after :- [IASTNode]]
   (->>
    (diff-by write-node-valueless before after)
    (map (partial-right select-keys [:original :revised]))
    (apply merge-with concat)))

(s/defn author-name [rev-commit :- RevCommit]
  (-> rev-commit .getAuthorIdent .getName))

(s/defn author-email [rev-commit :- RevCommit]
  (-> rev-commit .getAuthorIdent .getEmailAddress))

(s/defn added-atoms
  [srcs]
  (let [{_ :original new-atoms     :revised} (atom-map-diff (:atoms-before srcs) (:atoms-after srcs))
        {_ :original new-non-atoms :revised} (atom-seq-diff (:non-atoms-before srcs) (:non-atoms-after srcs))]
    {
     :rev-str (:rev-str srcs)
     :file (:file srcs)
     :added-atoms (frequencies-by :type new-atoms)
     :added-non-atoms (count new-non-atoms)
     :author-name  (->> srcs :rev-commit author-name)
     :author-email (->> srcs :rev-commit author-email)
     }))

'((->>
   (commits-with
    gcc-repo "41ac8cbf4e888a2d80aec9254ea56ea9823169fa"
    (fn [rev-commit]
      (->> rev-commit
           :srcs
           (filter #(and (:atoms-before %) (:atoms-after %)))
           (pmap #(with-timeout 200 (added-atoms %)))
           (remove empty?)
           (map prn)
           dorun
          (log-err (str "edit-lines " (->> rev-commit :srcs first :rev-str)) {})
          )))
       (log-to "tmp/atom-committers_gcc_2018-01-04_03-200-timeout.edn")
       time-mins
       ))

;; edn -> csv
'((->> ;"atom-committers_linux_2017-11-01_01.edn"
   "atom-committers_gcc_2017-10-31_1-3_clean.edn"
     read-data
     ;rest
     (group-by :author-name)
     (map-values (partial map #(dissoc % :author-name :author-email)))
     (map-values (partial map (partial map-values vector))) ;; wrap every value in vector
     (map-values (partial apply merge-with into)) ;; merge all the vectors
     (map-values (partial map-values
                          (fn [xs]
                            (if (number? (first xs))
                              (reduce + xs)          ;; sum numbers
                              (-> xs set count)))))  ;; count unique strings
     (map (fn [[k v]] (merge {:author-name k} v)))
     (maps-to-csv "src/analysis/data/atom-committers_gcc_2017-10-31_1-3.csv")
     ;(maps-to-csv "src/analysis/data/atom-committers_linux_2017-11-01_01.csv")
     ))
