(ns atom-finder.atom-patch
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.classifier :refer :all]
   [atom-finder.patch :refer :all]
   [atom-finder.atom-stats :refer :all]
   [clojail.core :refer [thunk-timeout]]
   [clojure.pprint :refer [pprint]]
   [clojure.data.csv :as csv]
   [clojure.java.io :as io]
   [clj-jgit.porcelain :as gitp]
   [clj-jgit.querying :as gitq]
   [clj-jgit.internal :as giti]
   [schema.core :as s]
   [swiss.arrows :refer :all]
   )
  (:import
   [atom_finder.classifier Atom]
   [org.eclipse.jgit.lib ObjectReader Repository]
   [org.eclipse.jgit.api Git]
   [org.eclipse.jgit.revwalk RevCommit RevCommitList RevWalk]
   [org.eclipse.jgit.treewalk TreeWalk filter.PathFilter]
   [org.eclipse.cdt.core.dom.ast IASTTranslationUnit IASTNode]
   [org.eclipse.cdt.internal.core.dom.parser ASTNode]
   )
  )

(defmacro with-timeout [time & body]
  `(try (thunk-timeout (fn [] ~@body) ~time :seconds)
        (catch java.util.concurrent.TimeoutException e#
          (do
            (errln (str "Killed operation when it exceded max duration of " ~time ", body: " '~@body))
            nil))))

(def AtomFinder (s/=> IASTTranslationUnit [IASTTranslationUnit]))
(def AtomFinders [(s/one AtomFinder "atom-finder") AtomFinder])
(def BeforeAfter [(s/one IASTTranslationUnit "before") (s/one IASTTranslationUnit "after")])
(def BeforeAfters [(s/one BeforeAfter "commit-file") BeforeAfter])

;
;(do (def repo gcc-repo)(def commit-hash "3bb246b3c2d11eb3f45fab3b4893d46a47d5f931")(def file-name "gcc/c-family/c-pretty-print.c"))
;(do (def repo gcc-repo)(def commit-hash "370e45b9887b6603911bbe1776c556d2404455bf")(def file-name "gcc/c-family/c-pretty-print.c"))
;(do (def repo  ag-repo)(def commit-hash "05be1eddca2bce1cb923afda2b6ab5e67faa248c")(def file-name "src/print.c"))
;(def atom-classifier conditional-atom?)
;(def atom-finder (->> atom-lookup :conditional :finder))
;(def parent-hash (commit-parent-hash repo commit-hash))
;(def rev-commit (first (gitq/rev-list repo)))

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

;(print (commit-file-source repo (find-rev-commit repo commit-hash) "gcc/testsuite/g++.dg/debug/dwarf2/integer-typedef.C"))
;(print (commit-file-source repo commit-hash "gcc/c-family/ChangeLog"))

;(commit-file-atom-count gcc-repo (find-rev-commit gcc-repo "3bb246b3c2d11eb3f45fab3b4893d46a47d5f931") "gcc/c-family/c-pretty-print.c" conditional-atom?)
;(before-after-data gcc-repo (find-rev-commit gcc-repo "3bb246b3c2d11eb3f45fab3b4893d46a47d5f931") "gcc/c-family/c-pretty-print.c")
;(commit-file-atom-count gcc-repo commit-hash "gcc/c-family/c-pretty-print.c" conditional-atom?)

(s/defn edited-files
  "which files were edited in commit"
  [repo rev-commit :- RevCommit]
  (->> rev-commit
       (gitq/changed-files repo)
       ;(with-timeout 5) ; TODO remove?
       (filter #(= (last %) :edit))
       (map first)
       ))

;(atom-removed-in-commit-file? repo commit-hash "gcc/c-family/ChangeLog" atom-classifier)
;(def commit-hash "97574c57cf26ace9b8609575bbab66465924fef7")
;(def file-name "gcc/c-family/ChangeLog")

;TODO use RevWalk instead of find-rev-commit
; http://stackoverflow.com/questions/28852698/how-do-i-get-the-tree-from-parent-commits-using-the-jgit-api
(s/defn parent-rev-commit
  [repo rev-commit :- RevCommit]
  (find-rev-commit repo (.name (.getParent rev-commit 0))))

(s/defn apply-before-after ;TODO ineffecient
  "parse a commit and it's parent and apply f to the root of both"
  [repo rev-commit :- RevCommit file-name f]
  (let [parent-commit (parent-rev-commit repo rev-commit)]
    [(f (parse-source (commit-file-source repo parent-commit file-name)))
     (f (parse-source (commit-file-source repo rev-commit file-name)))]))

(s/defn non-atoms
  "Given an AST and a map of all atoms, return a list of all non-atom nodes"
  [root found-atoms :- {s/Keyword [IASTNode]}]
  (let [ungrouped-atoms (set (apply concat (vals found-atoms)))]
    (remove ungrouped-atoms (flatten-tree root))))

(s/defn find-all-atoms-non-atoms :- {s/Keyword [IASTNode]}
  "Find all atoms and non-atoms in AST"
  [root :- IASTNode]
  (let [found-atoms (find-all-atoms root)]
    (merge found-atoms
           {:non-atoms (non-atoms root found-atoms)
            :all-nodes (flatten-tree root)})))

(s/defn build-srcs
  [file-name :- String source-before :- String source-after :- String]
  (let [ast-before (mem-tu file-name source-before)
        ast-after  (mem-tu file-name source-after)
        atoms-before (find-all-atoms ast-before)
        atoms-after (find-all-atoms ast-after)]
  {:ast-before       ast-before
   :ast-after        ast-after
   :atoms-before     atoms-before
   :atoms-after      atoms-after
   :non-atoms-before (non-atoms ast-before atoms-before)
   :non-atoms-after  (non-atoms ast-after atoms-after)
   :source-before    source-before
   :source-after     source-after
   }))

(def empty-srcs
  {:ast-before       nil
   :ast-after        nil
   :atoms-before     (map-values (constantly nil) atom-lookup)
   :atoms-after      (map-values (constantly nil) atom-lookup)
   :non-atoms-before nil
   :non-atoms-after  nil})

(s/defn before-after-data
  "Return the ast of changed files before/after a commit"
  [repo rev-commit :- RevCommit file-name]
  (let [parent-commit (parent-rev-commit repo rev-commit)
        patch-str     (gitq/changed-files-with-patch repo rev-commit)
        source-before (commit-file-source repo parent-commit file-name)
        source-after  (commit-file-source repo rev-commit file-name)]
    (merge
     {:file file-name
      :rev-str (.name rev-commit)
      :patch-str  patch-str
      }

     ;(if (not (c-file? file-name)) empty-srcs
     (build-srcs file-name source-before source-after))))

(defn atom-specific-srcs
  [srcs atom]
  (merge srcs
         {:atoms-before (->> srcs :ast-before ((:finder atom)))
          :atoms-after  (->> srcs :ast-after  ((:finder atom)))}))

(s/defn atoms-in-file-stats
  "Check multiple atoms in a single file"
  [atoms :- [Atom] srcs]
  (doall
   (for [atom atoms]
     (let [atom-src (atom-specific-srcs srcs atom)]
       (if (:ast-before atom-src)
         {:atom (:name atom)
          :stats (apply merge
                        (doall (for [[stat-name f] (atom-stats)]
                                 {stat-name (f atom-src atom)})))}
         {:atom (:name atom)})
     ))))

;(atoms-in-file-stats (vals (select-keys atom-lookup [:post-increment :literal-encoding]))
;                     {:ast-before (parse-source "int main() { int x = 1, y; y = x++; }")
;                      :ast-after (parse-source "int main() { int x = 1, y; y = x; x++; }")})

(s/defn commit-files-before-after
  "For every file changed in this commit, give both before and after ASTs"
  [repo rev-commit :- RevCommit]
  (log-err (str "commit-files-before-after " (.name rev-commit)) []
           (let [patches-str (gitq/changed-files-with-patch repo rev-commit)
                 file-patches (->> patches-str parse-diff (map #(vector (or (.getOldFile %1) (.getNewFile %1)) %1)) (into {}))]
             (for [filename (edited-files repo rev-commit)]
               (log-err (str "c-f-b-a " {:commit-hash (.name rev-commit) :file filename}) nil

                        (with-timeout 120
                          (merge (before-after-data repo rev-commit filename)
                                 {:patch (file-patches filename)
                                  :rev-commit rev-commit})))))))

(s/defn atoms-changed-in-commit ;:- {s/Str {s/Keyword BACounts}}
  [repo :- Git atoms :- [Atom] rev-commit :- RevCommit]
  (doall (for [commit-ba (commit-files-before-after repo rev-commit)]
       (merge (select-keys commit-ba [:file])
        {:atoms (atoms-in-file-stats atoms commit-ba)}))))

;(pprint (atoms-changed-in-commit gcc-repo atoms (find-rev-commit gcc-repo "c565e664faf3102b80218481ea50e7028ecd646e")))


(defn parse-commit-for-atom
  [repo atoms rev-commit]
  (let [commit-hash (.name rev-commit)]
    (->>
     (atoms-changed-in-commit repo atoms rev-commit)
     (array-map :rev-str commit-hash :files)
     doall
     (log-err commit-hash {:rev-str commit-hash})
     )))

;(pprint (parse-commit-for-atom gcc-repo atoms (find-rev-commit gcc-repo commit-hash)))

(defn atoms-changed-all-commits
  [repo atoms]
  (->>
   (gitq/rev-list repo)
   (pmap (partial parse-commit-for-atom repo atoms))
  ))

;(atoms-changed-all-commits gcc-repo atoms)
;(time (println (map (fn [x] 1) (gitq/rev-list gcc-repo))))
;(time (println (map (fn [x] 1) (take 200 (atoms-changed-all-commits gcc-repo (take 2 atoms))))))
; rev-list -> 2543
; 4   ->  4666
; 8   ->  4646
; 20  ->  5000
; 20  ->  4400
; 50  -> 22977
; 100 -> 28277
; 200 -> 36484
; 200 -> 31531
; 200 -> 24806
;(time (find-rev-commit gcc-repo commit-hash))
;(time (find-rev-commit gcc-repo old-commit-hash))
;(time (dotimes [n 10] (parse-commit-for-atom gcc-repo atoms (find-rev-commit gcc-repo commit-hash))))
;(time (dotimes [n 600] (parse-commit-for-atom gcc-repo atoms (find-rev-commit gcc-repo old-commit-hash))))

;(parse-commit-for-atom gcc-repo atoms (find-rev-commit gcc-repo commit-hash))

(defn log-atoms-changed-all-commits
  [filename repo atoms]
  (log-to filename
    (println "(")
    (->> atoms
         (atoms-changed-all-commits repo)
         (map prn)
         ;(take 10)
         dorun
         time)
    (println ")")
    ))

;(def filename "gcc-bugs-atoms_2017-03-28_200.edn")
;(def gcc-bugs (->> filename read-data (mapcat identity) (filter :rev-str)))
;(->> gcc-bugs add-convenience-columns (write-res-csv "gcc-bugs_2017-03-28_200.csv"))

(defn write-res-csv
  [filename flat-res]
  (with-open [out-file (io/writer filename)]
    (csv/write-csv out-file
                   [(->> flat-res first keys
                        (map #(subs (str %) 1)))])
    (csv/write-csv out-file (->> flat-res (map vals)))))

(defn map-all-commits
  ([f repo]
   (map-all-commits pmap f repo))
  ([mapper f repo] ; (f srcs)
  (->>
   (gitq/rev-list repo)
   (mapper (fn [rev-commit]
           (f {:rev-commit rev-commit
               :rev-str    (.name rev-commit)
               ;:srcs       (with-timeout 1 ; don't try a single commit for too long
               ;              (doall (commit-files-before-after repo rev-commit)))})))
               :srcs (commit-files-before-after repo rev-commit)})))
   )))

(defn commits-from
  [repo commit-hash]
  (->> commit-hash
       (rev-walk-from repo)
       (pmap (fn [rev-commit]
              (log-err (str "parsing - " (.name rev-commit)) nil
                       {:rev-commit rev-commit
                        :rev-str    (.name rev-commit)
                        :srcs (commit-files-before-after repo rev-commit)})))))

(defn map-all-commit-files
  ([f repo]
   (map-all-commit-files pmap f repo))
  ([mapper f repo] ; (f srcs)
   (->>
    (map-all-commits mapper f repo)
    (map :srcs)
    flatten1
    )))
