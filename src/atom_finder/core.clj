(ns atom-finder.core
  (:require [atom-finder.classifier :refer :all]
            [atom-finder.count-subtrees :refer :all]
            [atom-finder.util :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as string]
            ))

(defn -main
  [& args]

  (def root (tu (resource-path "logic-as-control-flow.c")))
  (def big-root (tu (expand-home "~/opt/src/github-top-c/php-src/ext/sqlite3/libsqlite/sqlite3.c")))
  (def github-top-c (expand-home "~/opt/src/github-top-c"))


  (->> big-root
       (filter-type "FieldReference")
       first
       (pap write)
       children
       (pap write)
       first
       pap
       write
  )

  (->> big-root
       (non-trivial-expression-parents 1)
       (filter #(instance? org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTFieldReference %))
       (map write)
       frequencies
       (sort-by last)
       pprint
       )

)
