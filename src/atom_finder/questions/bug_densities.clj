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
   [swiss.arrows :refer :all]
   )
  (:import
   [org.eclipse.jgit.api Git]
   [org.eclipse.jgit.treewalk CanonicalTreeParser]))

(defn bugzilla-ids
  [rev-commit]
  (->> rev-commit
       ;(#(string/join (.getShortMessage %1) (.getFullMessage %1)))
       .getFullMessage
       ;(prn)
       (re-seq #"(?:PR|pr).*?(?:(\S+)/)?(\d{5,9})")
       (#(for [[match branch id] %]
          {:branch branch :bug-id (Long/parseLong id)}))
       (group-by :bug-id)
       (map-values #(apply max-key (comp count :branch) %))
       vals
       set
  ))

;(re-seq #"(?:PR|pr).*?(?:(\S+)/)?(\d{5,})" "PR12345")

(defn atom-and-bug-counts
  "For a given rev-commit, count how many bugs (and atoms before) are in the file"
  [srcss]
  (log-err (str "atom-and-bug-counts " (:rev-str srcss)) {}
             {:rev-str     (->> srcss :rev-str)
              :n-bugs      (->> srcss :rev-commit bugzilla-ids count)
              :n-atoms     (->> srcss :srcs (map :atoms-before) (mapcat vals) flatten count)
              :n-non-atoms (->> srcss :srcs (map :non-atoms-before) flatten count)
              :time        (now)
              }))

'(->> "tmp/bug-densities-2017-07-20_2.txt"
     read-lines
     (map #(dissoc % :rev-str))
     (group-by (comp pos? :n-bugs))
     (map-values (partial reduce (partial merge-with +)))
     pprint)

(defn log-bug-atom-file-correlation
  "For each commit, how many bugs and how many atoms"
  []
  (-<>> gcc-repo
        (map-all-commits pmap #(update-in % [:srcs] (fn [files] (with-timeout 45 (doall files)))))
        (remove nil?)
        (map atom-and-bug-counts)
        (map prn)
        dorun
        (log-to "tmp/bug-densities-2017-09-15_0_log_err_45s.txt")
        time-mins))
