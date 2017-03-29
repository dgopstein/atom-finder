(ns atom-finder.atom-patch
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.constants :refer :all]
   [atom-finder.classifier :refer :all]
   [atom-finder.source-versions :refer :all]
   [atom-finder.patch :refer :all]
   [atom-finder.results-util :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.data.csv :as csv]
   [clojure.java.io :as io]
   [clj-jgit.porcelain :as gitp]
   [clj-jgit.querying :as gitq]
   [clj-jgit.internal :as giti]
   [schema.core :as s]
   )
  (:import
   [atom_finder.classifier Atom]
   [org.eclipse.jgit.lib ObjectReader Repository]
   [org.eclipse.jgit.api Git]
   [org.eclipse.jgit.revwalk RevCommit]
   [org.eclipse.jgit.treewalk TreeWalk filter.PathFilter]
   [org.eclipse.cdt.core.dom.ast IASTTranslationUnit IASTNode]
   [org.eclipse.cdt.internal.core.dom.parser ASTNode]
   )
  )

(def AtomFinder (s/=> IASTTranslationUnit [IASTTranslationUnit]))
(def AtomFinders [(s/one AtomFinder "atom-finder") AtomFinder])
(def BeforeAfter [(s/one IASTTranslationUnit "before") (s/one IASTTranslationUnit "after")])
(def BeforeAfters [(s/one BeforeAfter "commit-file") BeforeAfter])
;(def BACounts [(s/one s/Num "count-before") (s/one s/Num "count-after")])
(def BACounts {(s/required-key :count-before) s/Int (s/required-key :count-after) s/Int})

;
;(do (def repo gcc-repo)(def commit-hash "3bb246b3c2d11eb3f45fab3b4893d46a47d5f931")(def file-name "gcc/c-family/c-pretty-print.c"))
;(do (def repo  ag-repo)(def commit-hash "05be1eddca2bce1cb923afda2b6ab5e67faa248c")(def file-name "src/print.c"))
;(def atom-classifier conditional-atom?)
;(def atom-finder (->> atom-lookup :conditional :finder))
;(def parent-hash (commit-parent-hash repo commit-hash))
;(def rev-commit (first (gitq/rev-list repo)))

(defn object-loader-string
  "dump the contents of an ObjectLoader to a String"
  [loader]
  (->> loader .getBytes String.))

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
      (object-loader-string loader)
      )
  ))

;(print (commit-file-source repo (find-rev-commit repo commit-hash) "gcc/testsuite/g++.dg/debug/dwarf2/integer-typedef.C"))
;(print (commit-file-source repo commit-hash "gcc/c-family/ChangeLog"))

;(commit-file-atom-count gcc-repo (find-rev-commit gcc-repo "3bb246b3c2d11eb3f45fab3b4893d46a47d5f931") "gcc/c-family/c-pretty-print.c" conditional-atom?)
;(ast-before-after gcc-repo (find-rev-commit gcc-repo "3bb246b3c2d11eb3f45fab3b4893d46a47d5f931") "gcc/c-family/c-pretty-print.c")
;(commit-file-atom-count gcc-repo commit-hash "gcc/c-family/c-pretty-print.c" conditional-atom?)

(s/defn edited-files
  "which files were edited in commit"
  [repo rev-commit :- RevCommit]
  (->> rev-commit
       (gitq/changed-files repo)
       (filter #(= (last %) :edit))
       (map first)
       ))

;(atom-removed-in-commit-file? repo commit-hash "gcc/c-family/ChangeLog" atom-classifier)
;(def commit-hash "97574c57cf26ace9b8609575bbab66465924fef7")
;(def file-name "gcc/c-family/ChangeLog")

;(defn commit-parent-hash
;  [repo commit-hash]
;  (.name (first (.getParents (find-rev-commit repo commit-hash)))))

;TODO use RevWalk instead of find-rev-commit
; http://stackoverflow.com/questions/28852698/how-do-i-get-the-tree-from-parent-commits-using-the-jgit-api
(s/defn parent-rev-commit
  [repo rev-commit :- RevCommit]
  (find-rev-commit repo (.name (.getParent rev-commit 0))))

(s/defn apply-before-after
  "parse a commit and it's parent and apply f to the root of both"
  [repo rev-commit :- RevCommit file-name f]
  (let [parent-commit (parent-rev-commit repo rev-commit)]
    [(f (parse-source (commit-file-source repo parent-commit file-name)))
     (f (parse-source (commit-file-source repo rev-commit file-name)))]))

(s/defn ast-before-after
  "Return the ast of changed files before/after a commit"
  [repo rev-commit :- RevCommit file-name]
  (let [parent-commit (parent-rev-commit repo rev-commit)
        source-before (commit-file-source repo parent-commit file-name)
        source-after (commit-file-source repo rev-commit file-name)]
    {:file file-name
     :ast-before (parse-source source-before)
     :ast-after  (parse-source source-after)
     :source-chars-before (count source-before)
     :source-chars-after  (count source-after)}))

(s/defn atoms-in-file-counts ;{s/Keyword BACounts}
  "Check multiple atoms in a single file"
  [atoms :- [Atom] srcs]
  (for [atom atoms]
    {:atom (:name atom)
     :count-before (->> srcs :ast-before ((:finder atom)) count)
     :count-after  (->> srcs :ast-after  ((:finder atom)) count)
     }))

(s/defn commit-files-before-after
  "For every file changed in this commit, give both before and after ASTs"
  [repo rev-commit :- RevCommit]
  (map (partial ast-before-after repo rev-commit)
       (edited-files repo rev-commit)))

(s/defn atoms-changed-in-commit ;:- {s/Str {s/Keyword BACounts}}
  [repo :- Git atoms :- [Atom] rev-commit :- RevCommit]
  (for [commit-ba (commit-files-before-after repo rev-commit)
        atoms-counts (atoms-in-file-counts atoms commit-ba)]
       (merge (dissoc commit-ba :ast-before :ast-after) atoms-counts)))

;(pprint (atoms-changed-in-commit gcc-repo atoms (find-rev-commit gcc-repo "c565e664faf3102b80218481ea50e7028ecd646e")))

(defn parse-commit-for-atom
  [repo atoms rev-commit]
  (let [commit-hash (.name rev-commit)]
    (try
      (doall (for [atom (atoms-changed-in-commit repo atoms rev-commit)]
        (merge {:revstr commit-hash :bug-ids (bugzilla-ids rev-commit)} atom)))
      (catch Exception e (do (errln (str "-- exception parsing commit: \"" commit-hash "\"\n")) {:revstr commit-hash}))
      (catch Error e     (do (errln (str "-- error parsing commit: \""  commit-hash "\"\n")) {:revstr commit-hash})))
    ))

      (try (/ 1 0) (catch Exception e "oops"))
;(pprint (parse-commit-for-atom gcc-repo atoms (find-rev-commit gcc-repo "c565e664faf3102b80218481ea50e7028ecd646e")))

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

(defn log-atoms-changed-all-commits
  [filename repo atoms]
  (binding [*out* (clojure.java.io/writer filename)]
    (->> atoms
         (atoms-changed-all-commits repo)
         (map prn)
         ;(take 10)
         dorun
         time)))

;(def filename "gcc-bugs-atoms_2017-03-26_2_1000.edn")
;(def gcc-bugs (->> filename read-data (mapcat identity) (filter :revstr)))
;(->> gcc-bugs add-convenience-columns (write-res-csv "gcc-bugs_2017-03-26_2_1000.csv"))

(defn write-res-csv
  [filename flat-res]
  (with-open [out-file (io/writer filename)]
    (csv/write-csv out-file
                   [(->> flat-res first keys
                        (map #(subs (str %) 1)))])
    (csv/write-csv out-file (->> flat-res (map vals)))))

(defn add-convenience-columns
  [flat-res]
    (for [m flat-res]
      (merge m {:n-bugs (-> m :bug-ids count)})))

;(write-res-csv "gcc-bugs.csv" (take 10000 (add-convenience-colums flat-gcc-bugs)))

;(def old-commit-hash "151ad919455c7143abb03ba325d073e7f86523bc")
