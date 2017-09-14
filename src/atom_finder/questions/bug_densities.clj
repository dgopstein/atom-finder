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
  {:rev-str     (->> srcss :rev-str)
   :n-bugs      (->> srcss :rev-commit bugzilla-ids count)
   :n-atoms     (->> srcss :srcs (map :atoms-before) (mapcat vals) flatten count)
   :n-non-atoms (->> srcss :srcs (map :non-atoms-before) flatten count)
   :time        (now)
   })

'(->> "tmp/bug-densities-2017-07-20_2.txt"
     read-lines
     (map #(dissoc % :rev-str))
     (group-by (comp pos? :n-bugs))
     (map-values (partial reduce (partial merge-with +)))
     pprint)

(defn log-bug-atom-file-correlation
  "For each commit, how many bugs and how many atoms"
  []
  (let [max-files 1000]
    (-<>> gcc-repo
          (map-all-commits map (fn [hash] (update-in hash [:srcs] #(with-timeout 1 (doall %)))))
          ;(map-all-commits map identity)
          ;(filter #(> 1000 (count (edited-files gcc-repo (:rev-commit %))))) ; don't process huge commits (memory issues)
          ;(drop 1562)
          ;(map #(update-in % [:srcs] (fn [srcs] (take max-files srcs)))) ; truncate large commits
          ;(filter #(<= max-files (count (:srcs %)))) ; don't process truncated commits
          ;(drop 15)
          ;(take 3)
          ;(for [commit <>] ())
          ;(map (fn [commit] (with-timeout 1 (doall commit))))
          (remove nil?)
          (map atom-and-bug-counts)
          (map prn)
          dorun
          (log-to "tmp/bug-densities-2017-09-14_0.txt")
          time-mins)))

'(defn count-files-in-commits
  "For every commit in GCC how many files were edited"
  [gcc-repo]
  (->> gcc-repo
     (map-all-commits map identity)
     (map :rev-commit)
     (map (fn [rc] (let [files (edited-files gcc-repo rc)] [(.name rc) (count files) files])))
     (map prn)
     dorun
     (log-to "tmp/commit-files.edn")
     dorun
     time-mins))

'(->> gcc-repo
     (map-all-commits map identity)
     (map :rev-commit)
     first
     ppublic-methods)

;;
;; DiffFormatter formatter = new DiffFormatter( System.out );
;; formatter.setRepository( git.getRepository() );
;; AbstractTreeIterator commitTreeIterator = prepareTreeParser( git.getRepository(),  Constants.HEAD );
;; FileTreeIterator workTreeIterator = new FileTreeIterator( git.getRepository() );
;; List<DiffEntry> diffEntries = formatter.scan( commitTreeIterator, workTreeIterator );
;;
;; for( DiffEntry entry : diffEntries ) {
;;   System.out.println( "Entry: " + entry + ", from: " + entry.getOldId() + ", to: " + entry.getNewId() );
;;   formatter.format( entry );
;; }
;;


'(defn commit-tree-iterator
  "Create a TreeIterator from a RevCommit"
  [repo rev-commit]
(.newObjectReader (.getRepository repo))
  (prn repo)
  (prn rev-commit)
  (prn (.getObjectId (giti/new-tree-walk repo rev-commit) 0))
  (prn "ok")
  (.reset (CanonicalTreeParser.)
          (.newObjectReader (.getRepository repo))
          (.getObjectId (giti/new-tree-walk repo rev-commit) 0)))

; http://git.eclipse.org/c/jgit/jgit.git/tree/org.eclipse.jgit.test/tst/org/eclipse/jgit/api/DiffCommandTest.java
'((defn n-edited-files
  [repo revstr]
   (let [rc (find-rev-commit repo revstr)]
     (.size
      (doto (.diff gcc-repo)
        (.setNewTree (commit-tree-iterator repo rc))
        (.setOldTree (commit-tree-iterator repo (.getParent rc 0)))
       .call))
     )
  ) gcc-repo "acfa6993d9ef04204386ef349bdd025737cdb425")

'(->>
 (map (fn [x] (Thread/sleep 1000)) (range 2))
 (map #(time (dorun %)))
 dorun
 time-mins)
