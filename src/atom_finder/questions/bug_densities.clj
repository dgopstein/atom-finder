(ns atom-finder.questions.bug-densities
  (:require
   [clj-jgit.internal  :as giti]
   [clj-jgit.porcelain :as gitp]
   [clj-jgit.querying  :as gitq]
   [atom-finder.patch :refer :all]
   [atom-finder.atom-patch :refer :all]
   [atom-finder.constants :refer :all]
   [atom-finder.util :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as string]
   ))

(defn bugzilla-ids
  [rev-commit]
  (->> rev-commit
       ;(#(string/join (.getShortMessage %1) (.getFullMessage %1)))
       .getFullMessage
       ;(prn)
       (re-seq #"(?:PR|pr).*?(?:(\S+)/)?(\d{5,})")
       (#(for [[match branch id] %]
          {:branch branch :bug-id (Integer/parseInt id)}))
       (group-by :bug-id)
       (map-values #(apply max-key (comp count :branch) %))
       vals
       set
  ))

;(re-seq #"(?:PR|pr).*?(?:(\S+)/)?(\d{5,})" "PR12345")

(defn atom-and-bug-counts
  "For a given rev-commit, count how many bugs (and atoms before) are in the file"
  [srcss]
  (let [n-bugs (->> srcss :rev-commit bugzilla-ids count)
        n-atoms (->> srcss :srcs (map :atoms-before) (mapcat vals) flatten count)]
    {:rev-str (:rev-str srcss) :n-bugs n-bugs :n-atoms n-atoms}))

'(->> gcc-repo
    (map-all-commits atom-and-bug-counts)
    (take 2)
    (map prn)
    dorun
     ;(log-to "tmp/bug-densities-2017-07-20.txt")
    time-mins)
