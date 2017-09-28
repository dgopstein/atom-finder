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


'(-<>> gcc-repo
     ;(map-all-commit-files identity)
     ;(commits-from <> "5c929e16b8a3a96eb1ef8691fa7b88cb74754005") (mapcat :srcs)
     (commits-from <> "9ab8ac2684b1553dbd9bb656751515a3fb5c218c") (mapcat :srcs)
;     (def commits))
;(-<>> commits
      (pmap (fn [commit]
           (log-err (str "edit-lines " (:rev-str commit)) {} ;todo rev-str isn't working here?
             (merge (select-keys commit [:file :rev-str])
                    (edit-lines commit)
                    {:n-bugs (->> commit :rev-commit bugzilla-ids count)}))))
     (map prn)
     dorun
     (log-to "tmp/bug-lines-2017-09-26_2.txt")
     time-mins)
