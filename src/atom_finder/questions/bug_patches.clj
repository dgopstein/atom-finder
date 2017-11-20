;; How many atoms are in bug-fix commits vs non-bug-fix commits

(ns atom-finder.questions.bug-patches
  (:require
   [clj-jgit.internal  :as giti]
   [clj-jgit.porcelain :as gitp]
   [clj-jgit.querying  :as gitq]
   [atom-finder.patch :refer :all]
   [atom-finder.atom-patch :refer :all]
   [atom-finder.constants :refer :all]
   [atom-finder.util :refer :all]
   [atom-finder.questions.bug-densities :refer :all]
   [atom-finder.questions.edit-lines :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as string]
   [swiss.arrows :refer :all]
   )
  (:import
   [org.eclipse.jgit.api Git]
   [org.eclipse.jgit.treewalk CanonicalTreeParser]))

'((-<>> (commits-from gcc-repo)
     (mapcat :srcs)
     (pmap (fn [commit]
             (log-err (str "edit-lines " (:rev-str commit)) {} ;todo rev-str isn't working here?
                      (merge (select-keys commit [:file :rev-str])
                             (edit-line-counts commit)
                             {:n-bugs (->> commit :rev-commit bugzilla-ids count)}))))
     (map prn)
     dorun
     ;(log-to "tmp/bug-lines_gcc_2017-11-09_1.txt")
     time-mins))

'((-<>>
   "tmp/bug-lines_gcc_2017-11-09_02.txt"
   read-lines
   (map (fn [commit]
          (merge (dissoc commit :original :changed)
                 (map-values (fn [x] (if (Double/isFinite x) (float x) 0)) ; NaN -> 0
                             #_(merge
                              (merge-with safe-div (:changed commit) (:original commit)) ; rate of change of atoms
                              {:all-atoms (safe-div (reduce + (vals (:changed commit))) (reduce + (vals (:original commit))))} ; total atom change rate
                              )

                             (map-values #(safe-div % (->> commit :changed :non-atom)) (:changed commit))
                             )
                 {:all-changed (->> commit :changed vals (reduce +))}
                 )))
   ;(take 3)
   ;(map prn)
   (maps-to-csv "src/analysis/data/bug-lines_gcc_2017-11-09_02.csv")
   ))

