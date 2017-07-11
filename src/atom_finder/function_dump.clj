(ns atom-finder.function-dump
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.constants :refer :all]
   [atom-finder.classifier :refer :all]
   [atom-finder.location-dump :refer :all]
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

'(->>
 (parse-file "~/opt/src/gcc/libdecnumber/decContext.c")
 (find-all atoms-and-comments-and-functions)
 pprint
 )

(defn separate-by-function
  [records] ; {:type :omitted-curly-braces, :start-line 245, :end-line 246, :offset 10887, :length 104, :path [12 2 0]}

  (let [[functions others] (separate #(= :function (:type %)) records)
        in-function? (->> functions
                          (map (fn [m] [(:offset (pprn m)) (+ (:offset m) (:length m))]))
                          range-set)]
    (separate #(in-function? (:offset %)) others)))

'(->> records
     separate-by-function
     (map (partial map :type)))




