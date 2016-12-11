(ns atom-finder.atom-patch
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.constants :refer :all]
   [atom-finder.classifier :refer :all]
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
;(def file-name "gcc/testsuite/g++.dg/debug/dwarf2/integer-typedef.C")
;(def repo gcc-repo)
;(def atom-classifier conditional-atom?)

(defn rev-walk-commit
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
        rev-commit (rev-walk-commit repo commit-hash)
        tree      (.getTree rev-commit)
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

(commit-file-atom-count gcc-repo "3bb246b3c2d11eb3f45fab3b4893d46a47d5f931" "gcc/c-family/c-pretty-print.c" conditional-atom?)


(defn commit-file-atom-count
  "count the occurence of an atom in commit's version of file"
  [repo commit-hash file-name atom-classifier]
    (->> (commit-file-source repo commit-hash file-name)
         parse-source
         (atoms-in-tree atom-classifier)
;         count
         )
  )

(defn atom-removed-in-commit?
  [repo commit-hash atom-classifier]
  (some? #(atom-removed-in-commit-file? repo commit-hash % atom-classifier) (patch-files (gitq/changed-files-with-patch))))

(defn atom-removed-in-commit-file?
  [repo commit-hash file atom-classifier]
  (let [parent-hash (.getParent (find-commit repo commit-hash))]
      (< 0
         (- (commit-file-atom-count repo commit-hash file-name atom-classifier)
            (commit-file-atom-count repo parent-hash file-name atom-classifier)))))
