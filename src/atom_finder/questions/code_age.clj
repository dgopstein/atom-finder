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
   [org.eclipse.jgit
    api.Git lib.Repository lib.ObjectId
    revwalk.RevCommit revwalk.RevWalk
    treewalk.CanonicalTreeParser treewalk.TreeWalk]))

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

;; https://stackoverflow.com/questions/1685228/how-to-cat-a-file-in-jgit
(s/defn treewalk->filestr
  [repo :- Repository tree-walk :- TreeWalk]
  (-<>> tree-walk (.getObjectId <> 0) (.open repo) .getBytes String.))

(s/defn treewalk->file :- {:path s/Str :content s/Str}
  [repo :- Repository tree-walk :- TreeWalk]
  {:path    (->> tree-walk .getPathString)
   :content (->> tree-walk (treewalk->filestr repo))})

;; https://stackoverflow.com/questions/19941597/use-jgit-treewalk-to-list-files-and-folders
(def RevFile {:path s/Str :rev-str s/Str :content s/Str})
(s/defn repo-files :- [RevFile]
  "Return the content of every file in repository at given commit"
  [git-repo :- Git commit :- RevCommit]
  (let [repository (.getRepository git-repo)
        walk (org.eclipse.jgit.revwalk.RevWalk. repository)
        tree   (.getTree commit)
        tree-walk (org.eclipse.jgit.treewalk.TreeWalk. repository)
        ]

    (.addTree tree-walk tree)
    (.setRecursive tree-walk true)

    (take-while identity
                (repeatedly #(and (.next tree-walk)
                                  (assoc (treewalk->file repository tree-walk)
                                         :rev-str (->> commit .getId ObjectId/toString)))))))

'((-<>>
 gcc-repo
 first-monthly-commits
 first
 (repo-files gcc-repo)
 (take 2)
 pprint
 time-mins
 ))
