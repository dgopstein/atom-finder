(ns atom-finder.location-dump
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.constants :refer :all]
   [atom-finder.classifier :refer :all]
   [clojure.pprint :refer [pprint]]
   [schema.core :as s]
   )
  (:import
   [org.eclipse.cdt.core.dom.ast IASTNode]
   )
  )

(s/defn find-all :- [{:type s/Keyword s/Any s/Any}]
  "Apply every classifier to this node"
  [node :- IASTNode]
  (let [contexts (context-map node)]
    (->> atoms
         ((flip conj) {:name :comment :finder all-comments})
         (mapcat
          (fn [atom-map]
            (for [atom ((:finder atom-map) node)]
              (merge {:type (:name atom-map) :node atom}
                     (select-keys (loc atom) [:start-line :end-line :offset :length])
                     (contexts atom)
                     ))))
         )
    )
  )

(defn location-dump [filename]
  (map (partial merge {:file filename}) (->> filename parse-file find-all)))

;(->> "/Users/dgopstein/opt/src/gcc/contrib/paranoia.cc" location-dump (map prn))

'(->> gcc-path
     (pmap-dir-files location-dump)
     (mapcat (partial map #(update % :node write-node)))
     (map #(merge %1 (if (:path %1) {:depth (count (:path %1))} {})))
     (map prn)
     dorun
     (log-to "location-dump_2017-05-29.txt")
     )
