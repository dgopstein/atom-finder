(ns atom-finder.source-versions
  (:require
   [clj-jgit.porcelain :refer :all]
   [clj-jgit.querying :refer :all]
   [atom-finder.patch :refer :all]
   [atom-finder.util :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as string]
   ))

(def repo (load-repo (expand-home "~/opt/src/gcc")))

;; Log
(def rlog (git-log repo))

(def rev-walk (new-rev-walk repo))

(def rlist (rev-list repo))

(->> rlist
     (map (fn [rev] [rev (changed-files-with-patch repo rev)]))
     (take 200)
     (map (fn [[rev patch]]
       (prn [(-> rev .name) (context-lines patch)])
       ))
     )

(->> rlist
     (map (fn [rev] [rev (changed-files-with-patch repo rev)]))
     ;(some #(when (= (.name (first %1)) "36e1b0b39d209ff952526929dad4b230b94c29f7") %1))
     (take 4)
     (map println)
     )

(changed-files rev)
