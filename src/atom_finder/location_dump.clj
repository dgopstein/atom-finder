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
(->> big-root
     flatten-tree
     (map #(map offset (children %)))
     (take 10)
     ;(drop 1)
     (map (partial apply <= 0))
     (filter false?)
     count
     pprint)

(s/defn find-all :- [{:type s/Keyword s/Any s/Any}]
  "Apply every classifier to this node"
  [node :- IASTNode]
  (->> atoms
       ((flip conj) {:name :comment :finder all-comments})
       (mapcat
        (fn [atom-map]
          (for [atom ((with-context (:finder atom-map)) node)]
            (merge {:type (:name atom-map) :node atom} (select-keys (loc atom) [:start-line :end-line :offset :length])))))
       )
  )
(->> root find-all (map #(update % :node write-node)) (map prn))
(->> big-root
     ;((default-finder omitted-curly-braces-atom?))
     flatten-tree (take 1) (map classify-all)
     pprint)

(defn location-dump [filename]
  (let [root (parse-file filename)
        ]
    {:file filename
     :comments (->> root all-comments (map start-line))
     :atoms (->> atoms
     }
       )
  )

(->> gcc-path
     (pmap-dir-files location-dump)
     (map prn)
     (take 2)
     dorun)
