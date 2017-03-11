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

(->> "macro-operator-precedence_redis_2017-03-10_1.edn"
     read-data
     (take 30)
     (found-atom-source :macro-operator-precedence)
     ;sum-found-atoms
     )

;sum-found-atoms => {:macro-operator-precedence 1760}
;sum-found-atoms => {:macro-operator-precedence  759} # After removing atomic macros (#define M1 123)
