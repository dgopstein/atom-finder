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
        idxs (if (= 1 n)
               [(/ len 2)]
               (map #(Math/round (float (* (dec len) (/ % (dec n))))) (range n)))]
    (filter-by-index seq idxs)))


(defn sec->java-time
  [sec]
  (java.time.LocalDateTime/ofEpochSecond sec 0 (java.time.ZoneOffset/ofHours 0)))

(-<>>
 gcc-repo
 gitq/rev-list
 (take 10000)
 (map #(-<> % .getCommitTime long sec->java-time))
 (distinct-by (juxt (memfn getYear) (memfn getMonthValue)))
 ;(distinct-by #(-<> % .getCommitTime long sec->java-time ((juxt (memfn getYear) (memfn getMonthValue)))))
 (map prn)
 dorun
 time-mins
 )
