(ns atom-finder.questions.edit-lines
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.constants :refer :all]
   [atom-finder.classifier :refer :all]
   [atom-finder.patch :refer :all]
   [atom-finder.atom-stats :refer :all]
   [atom-finder.atom-patch :refer :all]
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

(defn start-lines-range-set
  "Returns a range-set of every line that contains the start of an AST node"
  [nodes]
  (->> nodes
       (map start-line)
       (remove nil?)
       range-set-canonical
  ))

(defn edit-lines
  "Is a line that contains at least one atom more likely to change
   (removed/edited [not added, because it didn't exist before])
   when edits are made?"
  [srcs]
  (let [old-change-ranges (->> srcs :patch patch-change-bounds flatten1
                               (map change-bound-to-ranges) (map :old))
        ;_ (pprn old-change-ranges)
        old-change-range-set (range-set-co old-change-ranges)
        ;_ (pprn old-change-range-set)
        atom-lines (->> srcs :atoms-before vals flatten start-lines-range-set)
        non-atom-lines (.difference (->> srcs :ast-before flatten-tree start-lines-range-set) atom-lines)
        changed-atom-lines (.intersection old-change-range-set atom-lines)
        changed-non-atom-lines (.intersection non-atom-lines old-change-range-set)
        ]

    (map-values count-range-set
                {:n-atom-lines atom-lines
                 :n-non-atom-lines non-atom-lines
                 :n-changed-atom-lines changed-atom-lines
                 :n-changed-non-atom-lines changed-non-atom-lines
                 :n-changed-lines old-change-range-set})))

'(->> gcc-repo
     (map-all-commit-files edit-lines)
     (map prn)
     (take 100)
     dorun
     (log-to "tmp/edit-lines-2017-07-18.txt")
     time-mins)

'(->> "tmp/edit-lines-2017-07-18.txt"
     read-lines
     (reduce (partial merge-with +))
     pprint)
