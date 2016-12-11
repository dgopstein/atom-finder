(ns atom-finder.atom-patch
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.constants :refer :all]
   [atom-finder.classifier :refer :all]
   [atom-finder.source-versions :refer :all]
   [atom-finder.patch :refer :all]
   [clojure.pprint :refer [pprint]]
   [clj-jgit.porcelain :as gitp]
   [clj-jgit.querying :as gitq]
   [clj-jgit.internal :as giti]
   )
  (:import
   [org.eclipse.jgit.lib ObjectReader]
   [org.eclipse.jgit.treewalk TreeWalk filter.PathFilter]
   )
  )

;(def commit-hash "3bb246b3c2d11eb3f45fab3b4893d46a47d5f931")
;(def file-name "gcc/c-family/c-pretty-print.c")
;(def repo gcc-repo)
;(def atom-classifier conditional-atom?)
;(def parent-hash (commit-parent-hash repo commit-hash))

(defn rev-commit
  "make a new revwalk to find given commit"
  [repo commit-hash]
  (gitq/find-rev-commit repo (giti/new-rev-walk repo) commit-hash)
  )

(defn object-loader-string
  "dump the contents of an ObjectLoader to a String"
  [loader]
  (->> loader .getBytes String.))

(defn commit-file-source
  "Return full source for each file changed in a commit"
  [repo commit-hash file-name]
  (let [repository (.getRepository repo)
        rc (rev-commit repo commit-hash)
        tree      (.getTree rc)
        tree-walk (doto (TreeWalk. repository) (.setRecursive true) (.addTree tree))
        ]
    
    (.setFilter tree-walk (PathFilter/create file-name)) ; Use PathFilterGroup???? http://download.eclipse.org/jgit/docs/jgit-2.0.0.201206130900-r/apidocs/org/eclipse/jgit/treewalk/filter/PathFilter.html
    (.next tree-walk)

    (let [object-id (.getObjectId tree-walk 0)
          loader (.open repository object-id)]
      (object-loader-string loader)
      )
  ))


;(print (commit-file-source repo commit-hash "gcc/testsuite/g++.dg/debug/dwarf2/integer-typedef.C"))
;(print (commit-file-source repo commit-hash "gcc/c-family/ChangeLog"))

;(commit-file-atom-count gcc-repo "3bb246b3c2d11eb3f45fab3b4893d46a47d5f931" "gcc/c-family/c-pretty-print.c" conditional-atom?)
;(commit-file-atom-count gcc-repo commit-hash "gcc/c-family/c-pretty-print.c" conditional-atom?)

(defn commit-file-atom-count
  "count the occurence of an atom in commit's version of file"
  [repo commit-hash file-name atom-classifier]
    (->> (commit-file-source repo commit-hash file-name)
         parse-source
         (atoms-in-tree atom-classifier)
         count
         ))

(defn edited-files
  "which files were edited in commit"
  [repo commit-hash]
  (->> (rev-commit repo commit-hash)
       (gitq/changed-files repo)
       (filter #(= (last %) :edit))
       (map first)
       ))


;(atoms-removed-in-commit repo commit-hash atom-classifier)
;(atom-removed-in-commit-file? repo commit-hash "gcc/c-family/ChangeLog" atom-classifier)
;(def commit-hash "97574c57cf26ace9b8609575bbab66465924fef7")
;(def file-name "gcc/c-family/ChangeLog")

(defn commit-parent-hash
  [repo commit-hash]
  (.name (first (.getParents (find-commit repo commit-hash)))))

(defn atom-removed-in-commit-file?
  [repo commit-hash file-name atom-classifier]
  (let [parent-hash (commit-parent-hash repo commit-hash)]
      (< (commit-file-atom-count repo commit-hash file-name atom-classifier)
         (commit-file-atom-count repo parent-hash file-name atom-classifier))))

(defn atoms-removed-in-commit
  [repo commit-hash atom-classifier]
  (into {}
        (map #(vector %1 (atom-removed-in-commit-file? repo commit-hash %1 atom-classifier))
             (edited-files repo commit-hash))))

(defn atom-removed-in-commit?
  [repo commit-hash atom-classifier]
  (any-true? #(true? (last %)) (atoms-removed-in-commit repo commit-hash atom-classifier)))


(defn atom-removed-all-commits
  [repo atom-classifier]

  (->> repo
       gitq/rev-list
       (pmap #(vector (.name %1)
                      (atom-removed-in-commit? repo %1 atom-classifier)
                      (bugzilla-ids %1)
                      ))
       )
  )

;(time (pprint (atom-removed-all-commits ag-repo conditional-atom?)))

;(binding [*out* (clojure.java.io/writer "gcc-conditional-commits.txt")]
;  (->> conditional-atom?
;       (atom-removed-all-commits gcc-repo)
;       (take 10)
;       (map println)
;       dorun
;       time))
