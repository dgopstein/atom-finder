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

;
;(do (def repo gcc-repo)(def commit-hash "3bb246b3c2d11eb3f45fab3b4893d46a47d5f931")(def file-name "gcc/c-family/c-pretty-print.c"))
;(do (def repo  ag-repo)(def commit-hash "05be1eddca2bce1cb923afda2b6ab5e67faa248c")(def file-name "src/print.c"))
;(def atom-classifier conditional-atom?)
;(def atom-finder (->> atom-lookup :conditional :find-all))
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
         count ; timing hot spot. Make more efficient atom counting function?
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

;(apply-before-after repo commit-hash file-name count-nodes)

(defn commit-parent-hash
  [repo commit-hash]
  (.name (first (.getParents (find-commit repo commit-hash)))))

(defn apply-before-after
  "parse a commit and it's parent and apply f to the root of both"
  [repo commit-hash file-name f]
  (let [parent-hash (commit-parent-hash repo commit-hash)]
    [(f (parse-source (commit-file-source repo parent-hash file-name)))
     (f (parse-source (commit-file-source repo commit-hash file-name)))]))

(defn source-before-after
  "Return the ast of changed files before/after a commit"
  [repo commit-hash file-name]
  (apply-before-after repo commit-hash file-name identity))

;(->> (source-before-after repo commit-hash file-name)
;     (map write-ast)
;     println)

(defn atom-removed-in-commit-file?
  [repo commit-hash file-name atom-classifier]
    (apply > (apply-before-after repo commit-hash file-name
                                 #(count (atoms-in-tree atom-classifier %)))))

(defn atom-removed-in-file?
  "Compare two sources for atom removal"
  [atom-finder srcs]
  (apply >
   (map (comp count atom-finder) srcs)))

(defn commit-files-before-after
  "For every file changed in this commit, give both before and after"
  [repo commit-hash]
  (->> (edited-files repo commit-hash)
       (map (partial source-before-after repo commit-hash))))

(defn atoms-removed-in-commit
  [repo commit-hash atom-classifier]
  (into {}
        (map #(vector %1 (atom-removed-in-commit-file? repo commit-hash %1 atom-classifier))
             (edited-files repo commit-hash))))

(defn atom-removed-in-commit?
  [repo commit-hash atom-classifier]
  (exists? #(true? (last %)) (atoms-removed-in-commit repo commit-hash atom-classifier)))
  ;(exists? (map (partial atom-removed-in-file? atom-finder)
  ;              (commit-files-before-after repo commit-hash))))

(defn parse-commit-for-atom
  [repo atom-classifier rev-commit]
  (let [commit-hash (.name rev-commit)]
    (try
      [commit-hash
       (atom-removed-in-commit? repo rev-commit atom-classifier)
       (bugzilla-ids rev-commit)
       ]
      (catch Exception e (do (printf "-- exception parsing commit: \"%s\"\n" commit-hash) [commit-hash nil nil]))
      (catch Error e     (do (printf "-- error parsing commit: \"%s\"\n" commit-hash) [commit-hash nil nil]))
      )))

(defn atom-removed-all-commits
  [repo atom-classifier]
  (pmap (partial parse-commit-for-atom repo atom-classifier)
   (gitq/rev-list repo)) ; TODO remove the take-nth
  )

;(atoms-removed-in-commit repo "5a59a1ad725b5e332521d0abd7f2f52ec9bb386d" conditional-atom?)

(defn log-atom-removed-all-commits
  []
  (binding [*out* (clojure.java.io/writer "gcc-logic-as-control-flow-commits.txt")]
    (->> ;conditional-atom?
         logic-as-control-flow-atom?
         (atom-removed-all-commits gcc-repo)
         ;(atom-removed-all-commits ag-repo)
         ;(atom-removed-all-commits (gitp/load-repo "/Volumes/RAM Disk/gcc"))
         ;(filter #(true? (nth % 1)))
         ;(take 3)
         (map println)
         (take 100)
         dorun
         time)))

(defn log-atoms-removed-all-commits
  [repo]
  (binding [*out* (clojure.java.io/writer "gcc-logic-as-control-flow-commits.txt")]
    (->> logic-as-control-flow-atom?
         (atom-removed-all-commits repo)
         (map println)
         (take 100)
         dorun
         time)))
