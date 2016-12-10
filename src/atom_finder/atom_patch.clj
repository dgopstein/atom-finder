(ns atom-finder.atom-patch
  (:require
   [atom-finder.util :refer :all]
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

(def commit-hash "3bb246b3c2d11eb3f45fab3b4893d46a47d5f931")
(def repo (gitp/load-repo (expand-home "~/opt/src/gcc")))

(defn commit-finder
  "Return full source for each file changed in a commit"
  [repo commit-hash]
  (let [object-reader (.. repo getRepository newObjectReader)
        object-loader (.open object-reader (giti/resolve-object commit-hash repo))
        object-stream (.openStream object-loader)]

    ;(.copyTo object-loader java.lang.System/out)
    (let [bytes (.getBytes object-loader)]
      (.release object-reader)
      (String. bytes)
    )
    ; (don't forget to release the ObjectReader when done) See also this article for more details
  ))

(defn rev-walk-commit
  "make a new revwalk to find given commit"
  [repo commit-hash]
  (gitq/find-rev-commit repo (giti/new-rev-walk repo) commit-hash)
  )

(defn object-loader-string
  "dump the contents of an ObjectLoader to a String"
  [loader]
  (->> loader .getBytes String.))

(defn commit-file
  "Return full source for each file changed in a commit"
  [repo commit-hash file-name]
  (let [repository (.getRepository repo)
        commit (giti/resolve-object commit-hash repo)
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


(print (commit-file repo commit-hash "gcc/testsuite/g++.dg/debug/dwarf2/integer-typedef.C"))
(print (commit-file repo commit-hash "integer-typedef.C"))
(print (commit-file repo commit-hash "gcc/c-family/ChangeLog"))

(defn atom-removed-in-commit?
  [repo commit-hash atom-classifier]

  )
