(ns atom-finder.source-versions
  (:require
   [clj-jgit.internal  :as giti]
   [clj-jgit.porcelain :as gitp]
   [clj-jgit.querying  :as gitq]
   [atom-finder.patch :refer :all]
   [atom-finder.util :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as string]
   ))

;(def repo (gitp/load-repo (expand-home "~/opt/src/gcc")))

;; Log
;(def rlog (gitp/git-log repo))

;(def rlist (gitq/rev-list repo))

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

;(bugzilla-ids (find-commit atom-finder.constants/gcc-repo "98103e4a9e8ae9e52751c9e96ec46e6095181b69"))

(defn find-rev-commit
  "make a new revwalk to find given commit"
  [repo commit-hash]
  (gitq/find-rev-commit repo (giti/new-rev-walk repo) commit-hash)
  )

;(doseq [revc (take 20 rlist)]
;  (println (.name revc))
;  (println (.getShortMessage revc))
;  (println "-----------------")
;  ;(println (.getFullMessage revc))
;  (println "\n\n\n")
;  )

;(pprint ;(filter #(> (count (last (prn %))) 0)
;                (take 2 (map #(vector (.name %1) (.getShortMessage %1)
;                                                                    ;(.getFullMessage %1)
;                                      (bugzilla-id %1)) rlist)))
;)

;(->> rlist
;     (map (fn [rev] [rev (gitq/changed-files-with-patch repo rev)]))
;     (take 200)
;     (map (fn [[rev patch]]
;       (prn [(-> rev .name) (context-lines patch)])
;       ))
;     )

;(->> rlist
;     (map (fn [rev] [rev (changed-files-with-patch repo rev)]))
;     ;(some #(when (= (.name (first %1)) "36e1b0b39d209ff952526929dad4b230b94c29f7") %1))
;     (take 4)
;     (map println)
;     )

