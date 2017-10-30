;; Do various projects have similar atom rates or
;; do different projects promote different coding styles

(ns atom-finder.questions.cross-project-rates
  (:require
   [atom-finder.atoms-in-dir :refer :all]
   [atom-finder.classifier :refer :all]
   [atom-finder.constants :refer :all]
   [atom-finder.util :refer :all]
   [clojure.pprint :refer :all]
   [clojure.string :as string]
   [swiss.arrows :refer :all]
   [schema.core :as s]
   ))


'((->>
 "atom-counts_2017-10-25_1.edn"
 read-data
 (drop 1)
 (def atom-counts)))

'((->>
 atom-counts
 (remove nil?)
 (group-by #(->> % :file (re-find #"[^/]*")))
 (map-values #(->> % (map :atom-counts) (reduce (partial merge-with +))))
 (def atom-sums)))

'((->>
 atom-sums
 (map (fn [[project counts]] (assoc counts :project project)))
 (maps-to-csv "atom-counts.csv")
 ))
