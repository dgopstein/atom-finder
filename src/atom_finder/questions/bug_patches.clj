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

'((-<>>
   ;;"9ab8ac2684b1553dbd9bb656751515a3fb5c218c"
   ;;"2355cca743fdf675a56964ae41de3357482cb480"
   ;;"d353bf189d2bbaf4059f402ee4d2a5ea074c349f"
   ;; Note - "Update copyright years." commits do not complete, every time
   ;;"e3afb61d668b6ae0e3ded679dbf3d9532347b406"
   ;; "4f8e39e244e8f9658772dbb440cf58d112022c54"
   ;;"5a5f1b4888d9cd093b608d0f547d50d2a4928908"
   ;;"1131b1ddb779908208eeed555fe09d5b98ec5f19"
   (commits-with
    gcc-repo
    "711789cc2a21e70e4ea0e2d9d5863924d85687d6"
    (fn [rev-commit]
      (doseq [commit (:srcs rev-commit)]
        (prn
         (log-err (str "edit-lines " (:rev-str commit)) {} ;todo rev-str isn't working here?
                  (merge (select-keys commit [:file :rev-str])
                         (with-timeout 5
                           (edit-line-counts commit))
                         {:n-bugs (->> commit :rev-commit bugzilla-ids count)}))))))
    (log-to "tmp/bug-lines_gcc_2018-01-04_02.txt")
    time-mins
    ))

'((-<>>
   "tmp/bug-lines_gcc_2017-11-09_combined.txt"
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
   (maps-to-csv "src/analysis/data/bug-lines_gcc_2017-11-09_combined.csv")
   time-mins
   ))
