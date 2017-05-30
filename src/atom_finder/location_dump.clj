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

(offset node)
(def node big-root)

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
'(->> atom-finder.constants/big-root
     find-all
     (map #(update % :node write-node))
     (map prn)
     time
     )

(defn location-dump [filename]
  (map (partial merge {:file filename}) (->> filename parse-file find-all)))

;(->> "/Users/dgopstein/opt/src/gcc/contrib/paranoia.cc" location-dump (map prn))

(->> gcc-path
     (pmap-dir-files location-dump)
     (map (partial map #(update % :node write-node)))
     (map (partial map prn))
     (take 2)
     dorun)
