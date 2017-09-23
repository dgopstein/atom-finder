(ns atom-finder.questions.atom-counts
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.constants :refer :all]
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

(->> "~/opt/src/linux-4.12.4"
      expand-home
      (pmap-dir-trees
       (fn [root]
         {:file (str/replace-first (.getFilePath root) #".*(?=linux)" "")
          :atom-counts (map-values #(count ((:finder %) root)) atom-lookup)}))
      (map prn)
      ;(take 1000)
      dorun
      (log-to "tmp/linux-4.12.4-atom-counts_2017-09-23_0.edn")
      time
      )

'(->> "linux-atom-counts_2017-07-06_01.edn"
     read-data
     (map :atom-counts)
     (reduce (partial merge-with +))
     count)
