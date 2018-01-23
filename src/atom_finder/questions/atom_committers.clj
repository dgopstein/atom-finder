;; What is the distribution of people who commit atoms?

(ns atom-finder.questions.atom-committers
  (:require
   [atom-finder.atoms-in-dir :refer :all]
   [atom-finder.atom-patch :refer :all]
   [atom-finder.classifier :refer :all]
   [atom-finder.constants :refer :all]
   [atom-finder.commits-added-removed :refer :all]
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

'((->>
   (commits-with
    gcc-repo
    "7deea7116aa99887d0a6524777c53e38015e1b50"
    (fn [rev-commit]
      (->> rev-commit
           :srcs
           (filter #(and (:atoms-before %) (:atoms-after %)))
           (pmap #(log-timeout 400 (str "added-atom-count " (select-keys % [:rev-str :file])) (added-removed-atoms-count %)))
           (remove empty?)
           (map prn)
           dorun
           (log-err "atom-committers " {})
          )))
       (log-to "tmp/atom-committers_gcc_2018-01-07_01-added-removed_3_continued.edn")
       time-mins
       ))

;; edn -> csv
'((->> ;"atom-committers_linux_2017-11-01_01.edn" read-data
   "tmp/atom-committers_gcc_2018-01-06_02_added-atoms-count.edn_partial" read-lines
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
     (maps-to-csv "src/analysis/data/atom-committers_gcc_2018-01-06_02_added-atoms-count.csv_partial")
     ))
