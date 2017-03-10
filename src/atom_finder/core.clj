(ns atom-finder.core
  (:require [atom-finder.classifier :refer :all]
            [atom-finder.count-subtrees :refer :all]
            [atom-finder.constants :refer :all]
            [atom-finder.util :refer :all]
            [atom-finder.classifier-util :refer :all]
            [atom-finder.results-util :refer :all]
            [atom-finder.atoms-in-dir :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            ))

(defn -main
  [& args]

  (print-atoms-in-dir
   (expand-home "~/opt/src/redis")
   (map atom-lookup [:macro-operator-precedence])
   )
)

(->> "macro-operator-precedence_redis_2017-03-07.edn"
     read-data
     ;(take 10)
     ;(found-atom-source :macro-operator-precedence)
     sum-found-atoms
     )

;sum-found-atoms => {:macro-operator-precedence 1760}
