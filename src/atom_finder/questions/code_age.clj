;; Does the prevelance of atoms change over time with a project
;; Projects started in the 80's vs projects started in the 2000's

(ns atom-finder.questions.code-age
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

;; https://stackoverflow.com/questions/7744656/how-do-i-filter-elements-from-a-sequence-based-on-indexes
(defn filter-by-index [coll idxs]
  (keep-indexed #(when ((set idxs) %1) %2)
                coll))

(defn distributed-sample
  "Find n samples equally distributed across the sequence [0 .. len*(1/n) .. len*(2/n) .. len*((n-1)/n)]"
  [n seq]
  (let [vec  (into [] seq)
        len  (count seq)
        idxs (map #(Math/round (float (* (dec len) (/ % (dec n))))) (range n))]
    (filter-by-index seq idxs)))

'(-<>>
 gcc-repo
 gitq/rev-list
 (distributed-sample 5)
 (map #(java.util.Date. (* 1000 (long (.getCommitTime %)))))
 pprint
 time-mins
 )
