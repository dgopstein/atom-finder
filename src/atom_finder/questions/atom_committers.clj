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

'((->>
   (commits-with
    gcc-repo
    "3bb22d5fa5f279e90cff387b5db4644a620b5576" 2
    (fn [rev-commit]
      (->> rev-commit
           :srcs
           (filter #(and (:atoms-before %) (:atoms-after %)))
           (pmap #(log-timeout 1 (str "added-atom-count " (select-keys % [:rev-str :file])) (added-atoms-count %)))
           (remove empty?)
           (map prn)
           dorun
          (log-err (str "atom-committers " (->> rev-commit :srcs first :rev-str)) {})
          )))
       ;(log-to "tmp/atom-committers_gcc_2018-01-06_added-atoms-local.edn")
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
