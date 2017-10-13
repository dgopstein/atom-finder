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
   [schema.core :as s]
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

(def year-month (juxt (memfn getYear) (memfn getMonthValue)))

(defn first-monthly-commits
  [repo]
  (->> repo gitq/rev-list
       (partition-by #(-<> % .getCommitTime long sec->java-time year-month))
       (map last)))

;; https://stackoverflow.com/questions/19941597/use-jgit-treewalk-to-list-files-and-folders
(s/defn list-repository-contents [git-repo :- Git commit]
  (let [repository (.getRepository git-repo)
        ;head (.getRef repository "HEAD") ;TODO NOT HEAD
        walk (org.eclipse.jgit.revwalk.RevWalk. repository) ; probably can be the commit
        ;commit (.parseCommit walk (.getObjectId head))
        tree   (.getTree commit)
        treeWalk (org.eclipse.jgit.treewalk.TreeWalk. repository)
        ]

    (.addTree treeWalk tree)
    (.setRecursive treeWalk true)

    (take-while identity (repeatedly #(and (.next treeWalk) (.getPathString treeWalk))))))

(-<>>
 gcc-repo
 first-monthly-commits
 first
 (list-repository-contents gcc-repo)
 (take 10)
 pprint
 time-mins
 )
