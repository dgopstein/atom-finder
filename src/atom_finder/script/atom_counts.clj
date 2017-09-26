(ns atom-finder.scripts.atom-counts
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

'(->> "~/opt/src/linux"
      expand-home
      (pmap-dir-trees
       (fn [root]
         {:file (str/replace-first (.getFilePath root) #".*(?=linux)" "")
          :atom-counts (map-values #(count ((:finder %) root)) atom-lookup)}))
      (map prn)
      dorun
      (log-to "tmp/linux-atom-counts_2017-07-06_01.edn")
      time
      )

(->> "linux-atom-counts_2017-09-23_1.edn"
     ;"gcc-atom-counts_2017-09-23_2.edn"
     read-data
     (map :atom-counts)
     (reduce (partial merge-with +))
     ((fn [h]
        (println (clojure.string/join "," (keys h) ))
        (println (clojure.string/join "," (vals h) ))))
     pprint)
