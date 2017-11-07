(ns atom-finder.atom-stats
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.constants :refer :all]
   [atom-finder.classifier :refer :all]
   [atom-finder.atom-stats :refer :all]
   [atom-finder.atoms-in-dir :refer :all]
   [clojure.pprint :refer [pprint]]
   [schema.core :as s]
   [swiss.arrows :refer :all]
   )
  )

(defn file-level-key? [key] (not (re-find #"atom" (str key))))

(def keep-numeric-vals (partial dissoc-by (comp not number? last)))

(s/defn sum-atom-counts
  [n-atoms :- s/Int map-list]; :- [{:ast-size-before s/Int s/Any s/Any}]]
  (->> map-list
       (map keep-numeric-vals)
       (apply merge-with +)
       ;(map-values-kv (fn [k v] (if (file-level-key? k) (long (/ v n-atoms)) v)))
       ))

(defn stats-by-file-type
  [flat-data]
  (let [n-atoms (->> flat-data (take 100) (map :atom) distinct (remove nil?) count)]
    (->> flat-data
         (group-by (comp file-ext :file))
         (map-values (partial sum-atom-counts n-atoms))
         ))
  )
