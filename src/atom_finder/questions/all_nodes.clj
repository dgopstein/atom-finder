;; How often is each type of AST node used in our corpus

(ns atom-finder.questions.all-nodes
  (:require
   [atom-finder.constants :refer :all]
   [atom-finder.util :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as string]
   [swiss.arrows :refer :all]
   [schema.core :as s]
   )
  )

(defn atom-finder-relative-path
  "Remove the ~/opt/src/atom-finder/ from the pathname"
  [path]
  (string/replace-first path (re-pattern (str atom-finder-path "/?")) ""))

(defn opname-or-typename
  "If the node is an expression, print out which kind, otherwise print out its type"
  [node]
  (-> node expr-operator :name (or (write-node-type node))))

'((prn (now)))
'((->> atom-finder-path
       (pmap-dir-files
        (fn [file]
          (assoc
           (->> file parse-file potential-atom-nodes (map opname-or-typename) frequencies)
           :file (atom-finder-relative-path file))))
       (map prn)
       dorun
       (log-to "tmp/all-node-counts_2018-01-22_opname.edn")
       time-mins
       ))

'((->>
   "tmp/all-node-counts_2018-01-22_opname.edn"
   read-lines
   (map (partial-right dissoc :file))
   (apply merge-with +)
   (sort-by (comp - last))
   (map (partial zipmap [:node-type :count]))
   (maps-to-csv "tmp/all-node-counts_2018-01-22_opname.csv")
   time-mins
   ))
