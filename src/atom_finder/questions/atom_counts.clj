;; For a single state (e.g. head, or a sable release) how many atoms
;; of each type exist in the codebase, relative to total AST nodes

(ns atom-finder.questions.atom-counts
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.constants :refer :all]
   [atom-finder.atom-patch :refer :all]
   [atom-finder.classifier :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.set :as set]
   [schema.core :as s]
   [clojure.data.csv :as csv]
   [clojure.string :as str]
   )
  (:import
   [org.eclipse.cdt.core.dom.ast IASTNode]
   )
  )

(defn count-atoms-in-tree
  [root]
  (map-values count (find-all-atoms-non-atoms root)))

(defn count-atoms-in-linux
  []
(->> "~/opt/src/atom-finder"
     ;(pap (constantly (now)))
      expand-home
      (pmap-dir-trees
       (fn [root]
         {:file (str/replace-first (.getFilePath root) #".*/atom-finder/" "")
          :atom-counts (count-atoms-in-tree root)}))
      (map prn)
      ;(take 10)
      dorun
      (log-to "tmp/atom-counts_2017-11-12_01.edn")
      time-mins
      )
  )

(defn summarize-atom-counts
  []
  ;(->> "gcc-atom-counts_2017-09-23_1.edn"
  (->> "linux-4.12.4-atom-counts_2017-09-23_0.edn"
     read-data
     (map :atom-counts)
     (reduce (partial merge-with +))
     pprint)
  )

;; Generate a full CSV of the data, useful for by-module or by-directory analysis
;; See src/analysis/directory-counts.R
'((->> "atom-counts_2017-10-25_1.edn"
     read-data
     rest
     (remove nil?)
     (map #(assoc (:atom-counts %1) :file (:file %1)))
     (maps-to-csv "src/analysis/data/atom-counts_2017-10-25_1.csv")
     ))
