(in-ns 'atom-finder.util)

(require '[clj-jgit.porcelain :as gitp]
         '[clj-jgit.querying  :as gitq]
         '[clj-jgit.internal  :as giti])

(import '(org.eclipse.jgit api.Git lib.Constants revwalk.RevCommit
                           treewalk.TreeWalk treewalk.filter.PathFilter))

(s/defn repo-head [repo :- Git] (-> repo .getRepository (.resolve Constants/HEAD)))

(defn find-rev-commit
  "make a new revwalk to find given commit"
  [repo commit-hash]
  (gitq/find-rev-commit repo (giti/new-rev-walk repo) commit-hash))

(defn rev-walk-from
  "make a new revwalk to starting at given commit"
  [repo commit-hash]
  (let [rev-walk (giti/new-rev-walk repo)]
    (.markStart rev-walk (gitq/find-rev-commit repo rev-walk commit-hash))
    rev-walk))

;; see code_age:repo-files
(s/defn commit-file-source :- String
  "Return full source for each file changed in a commit"
  [repo :- Git rev-commit :- RevCommit file-name :- String]
  (let [repository (.getRepository repo)
        tree       (.getTree rev-commit)
        tree-walk  (doto (TreeWalk. repository) (.setRecursive true) (.addTree tree))
        ]

    (.setFilter tree-walk (PathFilter/create file-name)) ; Use PathFilterGroup???? http://download.eclipse.org/jgit/docs/jgit-2.0.0.201206130900-r/apidocs/org/eclipse/jgit/treewalk/filter/PathFilter.html
    (.next tree-walk)

    (let [object-id (.getObjectId tree-walk 0)
          loader (.open repository object-id)]
      (->> loader .getBytes String.)
      )
  ))

(s/defn edited-files
  "which files were edited in commit"
  [repo rev-commit :- RevCommit]
  (->> rev-commit
       (gitq/changed-files repo)
       ;(with-timeout 5) ; TODO remove?
       (filter #(= (last %) :edit))
       (map first)
       ))

(s/defn parent-rev-commit
  [repo rev-commit :- RevCommit]
  (find-rev-commit repo (.name (.getParent rev-commit 0))))


;(do (def repo gcc-repo)(def commit-hash "3bb246b3c2d11eb3f45fab3b4893d46a47d5f931")(def file-name "gcc/c-family/c-pretty-print.c"))
;(do (def repo gcc-repo)(def commit-hash "370e45b9887b6603911bbe1776c556d2404455bf")(def file-name "gcc/c-family/c-pretty-print.c"))
;(do (def repo  ag-repo)(def commit-hash "05be1eddca2bce1cb923afda2b6ab5e67faa248c")(def file-name "src/print.c"))
;(def atom-classifier conditional-atom?)
;(def atom-finder (->> atom-lookup :conditional :finder))
;(def parent-hash (commit-parent-hash repo commit-hash))
;(def rev-commit (first (gitq/rev-list repo)))

;(print (commit-file-source repo (find-rev-commit repo commit-hash) "gcc/testsuite/g++.dg/debug/dwarf2/integer-typedef.C"))
;(print (commit-file-source repo commit-hash "gcc/c-family/ChangeLog"))

;(commit-file-atom-count gcc-repo (find-rev-commit gcc-repo "3bb246b3c2d11eb3f45fab3b4893d46a47d5f931") "gcc/c-family/c-pretty-print.c" conditional-atom?)
;(before-after-data gcc-repo (find-rev-commit gcc-repo "3bb246b3c2d11eb3f45fab3b4893d46a47d5f931") "gcc/c-family/c-pretty-print.c")
;(commit-file-atom-count gcc-repo commit-hash "gcc/c-family/c-pretty-print.c" conditional-atom?)

;(atom-removed-in-commit-file? repo commit-hash "gcc/c-family/ChangeLog" atom-classifier)
;(def commit-hash "97574c57cf26ace9b8609575bbab66465924fef7")
;(def file-name "gcc/c-family/ChangeLog")

;TODO use RevWalk instead of find-rev-commit
; http://stackoverflow.com/questions/28852698/how-do-i-get-the-tree-from-parent-commits-using-the-jgit-api
