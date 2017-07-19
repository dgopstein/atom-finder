(ns atom-finder.edit-lines
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.constants :refer :all]
   [atom-finder.classifier :refer :all]
   [atom-finder.source-versions :refer :all]
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
   [swiss.arrows :refer :all]
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

(defn map-all-commits
  [f repo] ; (f srcs)
  (->>
   (gitq/rev-list repo)
   (drop 4)
   (take 4)
   ;p
   (map (fn [rev-commit]
           (doall (map f (commit-files-before-after repo rev-commit)))))
   flatten1
   )
  )

(defn edit-lines
  "Is a line that contains at least one atom more likely to change when edits are made?"
  [srcs]
  (let [old-change-ranges (->> srcs :patch patch-change-bounds flatten1
                               (map change-bound-to-ranges) (map :old))
        ;_ (pprn old-change-ranges)
        old-change-range-set (range-set-co old-change-ranges)
        ;_ (pprn old-change-range-set)
        atom-lines (->> srcs :atoms-before vals flatten (map start-line) (map (fn [s] [s s])) range-set-cc)
        non-atom-lines (.difference (->> srcs :source-before count-lines long (#(range-set-oc [[0 %]]))) atom-lines)
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
     (map-all-commits edit-lines)
     (map pprint)
     dorun
     time-mins)
