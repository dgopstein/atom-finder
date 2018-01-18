;; Does the prevalence of atoms change over time within a project
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
    treewalk.CanonicalTreeParser treewalk.TreeWalk treewalk.filter.PathFilter]))

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
(defmethod year-month java.util.Date
  [date]
  (-> date .toInstant (.atZone (java.time.ZoneId/of "UTC")) .toLocalDateTime year-month))
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
(s/defn treewalk :- org.eclipse.jgit.treewalk.TreeWalk
  "Return the content of every file in repository at given commit"
  [git-repo :- Git commit :- RevCommit]
  (let [repository (.getRepository git-repo)
        tree-walk (org.eclipse.jgit.treewalk.TreeWalk. repository)]

    (doto tree-walk
      (.addTree (.getTree commit))
      (.setRecursive true))))

;; see atom-patch:commit-file-source
(s/defn repo-files :- [RevFile]
  "Return the content of every file in repository at given commit"
  [git-repo :- Git commit :- RevCommit]
  (let [tree-walk (treewalk git-repo commit)]
    (take-while identity
                (repeatedly #(and (.next tree-walk)
                                  (merge {:rev-str (->> commit .getId ObjectId/toString)
                                          :date (->> commit commit-time ymd-str)}
                                         (treewalk->file (.getRepository git-repo) tree-walk)))))))

(s/defn rev-filestr :- s/Str
  "Find the text of a file at a given revision"
  [repo :- Git rev-str :- s/Str path :- s/Str]
  (let [tree-walk (->> rev-str (find-rev-commit repo) (treewalk repo))]
    (treewalk->filestr
     (.getRepository repo)
     (doto tree-walk
       (.setFilter (PathFilter/create path))
       .next))))

(s/defn rev-filestr :- s/Str
  "Find the text of a file at a given revision"
  [repo :- Git rev-str :- s/Str path :- s/Str]
  (let [repository (.getRepository repo)]
    (->> rev-str (find-rev-commit repo) .getTree .getId
         (TreeWalk/forPath repository path)
         (treewalk->filestr repository))))

(defn monotize-by
  "filter out non-monotonic values"
  [cmp f coll]
  (->> coll (partition 2 1) (filter (fn [[a b]] (cmp (f a) (f b)))) (map first)))

'((def code-age-repo (->> "~/opt/src/linux-historical" expand-home gitp/load-repo)))
'((def code-age-repo (->> "~/opt/src/atom-finder/gcc" expand-home gitp/load-repo)))
;(def code-age-repo linux-historical-repo)

(->> "~/opt/src/atom-finder-histories"
     expand-home
     list-dirs
     (map #(juxt->> % .getName gitp/load-repo))
     (into {})
     (def history-repos))

'((print (now)))
'((-<>>
   "first-commits-per-month.txt"
   read-data
   (partition-by (fn [line] (-> line :date year-month (update-in [1] #(int (/ (dec %) 3))))))
   (map first)
   (map (fn [h]
          (let [repo (-> h :project history-repos)]
                {:project (:project h) :repo repo :rev-commit (find-rev-commit repo (-> h :rev-str))})))
   (mapcat (fn [h] (map (partial-right assoc :project (:project h)) (repo-files (:repo h) (:rev-commit h)))))
 (filter (comp c-file? :path))
 (pmap (fn [rev-file]
    (log-err (str "parsing/finding atoms in " (juxt->> rev-file :path :rev-str)) nil
             (with-timeout 120
        (if-let [ast (mem-tu (:path rev-file) (:content rev-file))]
          (assoc (dissoc rev-file :content)
                 :atoms (->> ast find-all-atoms-non-atoms (map-values count))))))))
 (map prn)
 dorun
 (log-to "tmp/code-age_all_2018-01-15_02_with-project.edn")
 time-mins
 ))

'((->>
   "tmp/code-age_gcc_2017-11-05_01_no-macro-exps.edn"
   read-lines
   (filter :path)
   (remove #(->> % :path (re-find #"test\/|\/test")))
   (group-by (juxt :date :rev-str))
   (map-values #(->> % (map :atoms) (apply merge-with +)))
   (map-values-kv (fn [[date rev-str] v] (merge v {:date date :rev-str rev-str})))
   vals
   (filter :date)
   (sort-by :date)
   reverse
   (maps-to-csv "atoms-by-month_gcc_2017-11-05_01_no-macro-exps.csv")
   time-mins
   ))


