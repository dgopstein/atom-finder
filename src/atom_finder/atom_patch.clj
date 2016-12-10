(ns atom-finder.atom-patch
  (:require
   [atom-finder.util :refer :all]
   [clojure.pprint :refer [pprint]]
   [clj-jgit.porcelain :as gitp]
   [clj-jgit.internal :as giti]
   )
  (:import
   [org.eclipse.jgit.lib ObjectReader]
   )
  )

(def commit-hash "3bb246b3c2d11eb3f45fab3b4893d46a47d5f931")
(def repo (gitp/load-repo (expand-home "~/opt/src/gcc")))

(defn commit-files
  "Return full source for each file changed in a commit"
  [repo commit-hash]
  (let [object-reader (.. repo getRepository newObjectReader)
        object-loader (.open object-reader (giti/resolve-object commit-hash repo))
        object-stream (.openStream object-loader)]

    (.copyTo object-loader java.lang.System/out)
    (.release object-reader)
    ; (don't forget to release the ObjectReader when done) See also this article for more details
  ))

(commit-files repo commit-hash)

(defn atom-removed-in-commit?
  [repo commit-hash atom-classifier]

  )
