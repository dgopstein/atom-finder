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


(-<>> gcc-repo
     (map-all-commit-files identity)
     ;(take 10)
;     (def commits))
;(-<>> commits
      (map (fn [commit]
             (merge (select-keys commit [:file :rev-str])
                    (edit-lines commit)
                    {:n-bugs (->> commit :rev-commit bugzilla-ids count)})))
     (map prn)
     dorun
     (log-to "tmp/bug-lines-2017-09-26.txt")
     time-mins)
