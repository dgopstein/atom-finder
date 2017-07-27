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
   ))

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
  {:rev-str     (->> srcss :rev-str)
   :n-bugs      (->> srcss :rev-commit bugzilla-ids count)
   :n-atoms     (->> srcss :srcs (map :atoms-before) (mapcat vals) flatten count)
   :n-non-atoms (->> srcss :srcs (map :non-atoms-before) flatten count)
   })

'(->> gcc-repo
    (map-all-commits map atom-and-bug-counts)
    (map prn)
    (take 3)
    dorun
    ;(log-to "tmp/bug-densities-2017-07-20_3.txt")
    time-mins)

'(->> "tmp/bug-densities-2017-07-20_2.txt"
     read-lines
     (map #(dissoc % :rev-str))
     (group-by (comp pos? :n-bugs))
     (map-values (partial reduce (partial merge-with +)))
     pprint)

'(-<>>
 (find-rev-commit gcc-repo "f934007a13a700b07d6d7473146cc51ccf5afff1")
 ;(edited-files gcc-repo)
 ;(commit-file-source gcc-repo <> "gcc/cp/pt.c")
 ;(before-after-data gcc-repo <> "gcc/cp/pt.c")
 (commit-files-before-after gcc-repo <>)
 (map keys)
 prn
 time-mins)

;; test code to generate an out-of-memory error (reduce -Xmx for faster results)
'(-<>>
 (range)
 (pap (fn [_] (used-memory)))
 (map (fn [i] [i (map #(parse-file (str gcc-path %)) ["/gcc/cp/pt.c" "/gcc/config/i386/i386.c"])]))
 (map #(and (get-in-tree [0] (last (last %1))) (println (first %1) ": " (used-memory))))
 (take 200)
 (dorun)
 prn
 ((fn [_] (println (used-memory))))
 time-mins
 )

