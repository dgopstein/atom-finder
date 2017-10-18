;; Does the prevelance of atoms change over time with a project
;; Projects started in the 80's vs projects started in the 2000's

(ns atom-finder.questions.code-age
  (:require
   [clj-jgit.internal  :as giti]
   [clj-jgit.porcelain :as gitp]
   [clj-jgit.querying  :as gitq]
   [atom-finder.patch :refer :all]
   [atom-finder.atom-patch :refer :all]
   [atom-finder.atoms-in-dir :refer :all]
   [atom-finder.classifier :refer :all]
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

(s/defn date->java-time
  [date :- java.util.Date]
  (.toLocalDateTime (.atZone (.toInstant date) (java.time.ZoneOffset/ofHours 0))))

(defn sec->java-time
  [sec]
  (java.time.LocalDateTime/ofEpochSecond sec 0 (java.time.ZoneOffset/ofHours 0)))

(s/defn commit-time
  [rc :- RevCommit]
  ;(->> rc .getCommitTime long sec->java-time))
  (->> rc .getAuthorIdent .getWhen date->java-time)) ; respects retroactive commits

(defn ymd-str [date]
  (.format date (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd")))

(defn year-month-day [date]
  (juxt->> date .getYear .getMonthValue .getDayOfMonth))

(defmulti year-month class)
(defmethod year-month java.time.LocalDateTime
  [date]
  (juxt->> date .getYear .getMonthValue))
(defmethod year-month RevCommit
  [rc]
  (->> rc commit-time year-month))

(defn first-monthly-commits
  [repo]
  (->> repo gitq/rev-list
       (partition-by year-month)
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
(def RevFile {:path s/Str :rev-str s/Str :content s/Str s/Any s/Any})
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
                                  (merge {:rev-str (->> commit .getId ObjectId/toString)
                                          :date (->> commit commit-time ymd-str)}
                                         (treewalk->file repository tree-walk)))))))

(defn monotize-by
  "filter out non-monotonic values"
  [cmp f coll]
  (->> coll (partition 2 1) (filter (fn [[a b]] (cmp (f a) (f b)))) (map first)))

'((def linux-historical-repo (->> "~/opt/src/linux-historical" expand-home gitp/load-repo)))

'((-<>>
 ["981a2edd1922c00e747680f30734ea50c86af28d"
  "747aead34de65c25765da79825ce2c08d8257b10"
  "1737992523930995ae7ee67cc9da87952c01aebc"
  "9b75471204ab8339eee7156e8f93959b9cfb0347"]
 (map (partial rev-walk-from linux-historical-repo))
 ;(distributed-sample 10) (map year-month) )
 (apply concat)
 (pap (constantly (now)))
 (partition-by (fn [rc] (-> rc year-month (update-in [1] #(int (/ (dec %) 3))))))
 (map last)
 (monotize-by (comp (partial < 0) compare) year-month)
 ;(map year-month) pprint)
 (mapcat (partial repo-files linux-historical-repo))
 (filter (comp c-file? :path))
 ;(drop 6100)
 ;(map (partial pap #(select-keys % [:rev-str :path])))
 (pmap (fn [rev-file]
    (log-err (str "parsing/finding atoms in " (juxt->> rev-file :path :rev-str)) nil
             (with-timeout 120
        (if-let [ast (mem-tu (:path rev-file) (:content rev-file))]
          (assoc (dissoc rev-file :content)
                 :atoms (->> ast find-all-atoms-non-atoms (map-values count))))))))
 ;(take 3)
 (map prn)
 dorun
 (log-to "tmp/code-age_linux-historical_2017-10-16_01_ungrafted.edn")
 time-mins
 ))
