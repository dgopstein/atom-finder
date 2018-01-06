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

(s/defn intersects-line-range-set?
  [range-set node :- IASTNode]
  (.intersects range-set
               (com.google.common.collect.Range/closed
                (start-line node) (end-line node))))

(s/defn added-atoms-local
  "Ignore parts of the files not contained in the patch"
  [srcs]
  (let [patch-bounds (-<>> srcs :patch-str (patch-file <> (:file srcs))
                           patch-change-bounds flatten1 (map change-bound-to-ranges))
        [old-bounds new-bounds] (->> patch-bounds (map #(select-values % [:old :new]))
                                     transpose (map range-set-co))
        {_ :original new-atoms     :revised}
          (atom-map-diff
           (->> srcs :atoms-before (map-values (partial filter #(intersects-line-range-set? old-bounds %))))
           (->> srcs :atoms-after  (map-values (partial filter #(intersects-line-range-set? new-bounds %)))))
        {_ :original new-non-atoms :revised}
        (atom-seq-diff
         (->> srcs :non-atoms-before (filter #(intersects-line-range-set? old-bounds %)))
         (->> srcs :non-atoms-after  (filter #(intersects-line-range-set? new-bounds %))))]

    ;(->> srcs :non-atoms-before count prn)
    ;(->> srcs :non-atoms-before (map (juxt start-line end-line)) prn)
    ;(pprn old-bounds)
    ;(->> srcs :non-atoms-before (filter #(intersects-line-range-set? old-bounds %)) (map (juxt start-line end-line)) prn)

    {
     :rev-str (:rev-str srcs)
     :file (:file srcs)
     :added-atoms (frequencies-by :type new-atoms)
     :added-non-atoms (count new-non-atoms)
     :author-name  (->> srcs :rev-commit author-name)
     :author-email (->> srcs :rev-commit author-email)
     }))

(->>
   snprintf_lite-src
   added-atoms-local
   pprint
   time-mins
)

'((def cp_pt-src (build-srcs "cp_pt.c"
                   (slurp-resource "gcc_cp_pt.c_92884c107e041201b33c5d4196fe756c716e8a0c")
                   (slurp-resource "gcc_cp_pt.c_d430756d2dbcc396347bd60d205ed987716b5ae8"))))

;'((def snprintf_lite-src
;    (merge
;     (build-srcs "snprintf_lite.cc"
;                 (slurp-resource "gcc_snprintf_lite.cc_1_aad93da1a579b9ae23ede6b9cf8523360f0a08b4")
;                 (slurp-resource "gcc_snprintf_lite.cc_2_3bb22d5fa5f279e90cff387b5db4644a620b5576"))
;     {:patch (->> "patch/aad93da1a579b9ae23ede6b9cf8523360f0a08b4_snprintf_lite.cc.patch"
;                  slurp-resource
;                  parse-diff)
;      :rev-commit (find-rev-commit gcc-repo "aad93da1a579b9ae23ede6b9cf8523360f0a08b4")})))

'((def snprintf_lite-src
    (let [rev-commit (find-rev-commit gcc-repo "3bb22d5fa5f279e90cff387b5db4644a620b5576")]
      (merge
       (before-after-data gcc-repo rev-commit "libstdc++-v3/src/c++11/snprintf_lite.cc")
       {:rev-commit rev-commit}))))


'((->>
   (commits-with
    gcc-repo
    (fn [rev-commit]
      (->> rev-commit
           :srcs
           (filter #(and (:atoms-before %) (:atoms-after %)))
           (pmap #(with-timeout 200 (added-atoms %)))
           (remove empty?)
           (map prn)
           dorun
          (log-err (str "atom-committers " (->> rev-commit :srcs first :rev-str)) {})
          )))
       (log-to "tmp/atom-committers_gcc_2018-01-04_XX-test-data.edn")
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
