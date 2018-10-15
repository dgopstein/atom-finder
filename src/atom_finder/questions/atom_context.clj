;; Inside which nodes are atoms found

(ns atom-finder.questions.atom-context
  (:require
   [atom-finder.constants :refer :all]
   [atom-finder.util :refer :all]
   [atom-finder.questions.question-util :refer :all]
   [clj-cdt.clj-cdt :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as string]
   [swiss.arrows :refer :all]
   [schema.core :as s]
   )
  (:import [org.eclipse.cdt.core.dom.ast IASTNode])
  )

;; dump all atoms and their node type to a file
(s/defn atom-context [edn-file]
  (println (str (now)))
  (-<>> atom-finder-corpus-path
        (pmap-dir-trees atom-finder.classifier/find-all-atoms)
        (remove nil?)
        ;;(take 3)
        (mapcat (s/fn [file-nodes :- {s/Keyword [org.eclipse.cdt.core.dom.ast.IASTNode]}]
                  (for [[atom-type nodes] file-nodes
                        node              nodes]
                    {:file (atom-finder-relative-path (filename node))
                     :line (start-line node)
                     :offset (offset node)
                     :atom atom-type
                     :node-type (opname-or-typename node)
                     :parent-type (some->> node parent opname-or-typename)})))
        (map prn)
        dorun
        (log-to edn-file)
        time-mins
  ))

(defn csv-atom-context [edn-file csv-file]
  (->> edn-file
       read-lines
       (map (partial map-values #(if (keyword? %) (name %) %)))
       (maps-to-csv csv-file)
       ))

(defn main-atom-context []
  (let [edn-file "tmp/atom-context_2018-10-12_parent-type.edn"
        csv-file "src/analysis/data/atom-context_2018-10-12_parent-type.csv"]
    (atom-context edn-file)
    (csv-atom-context edn-file csv-file)
    ))
