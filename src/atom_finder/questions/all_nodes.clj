;; How often is each type of AST node used in our corpus

(ns atom-finder.questions.all-nodes
  (:require
   [atom-finder.constants :refer :all]
   [atom-finder.util :refer :all]
   [atom-finder.atom-patch :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as string]
   [swiss.arrows :refer :all]
   [schema.core :as s]
   )
  )

(defn atom-finder-relative-path
  "Remove the ~/opt/src/atom-finder/ from the pathname"
  [path]
  (string/replace-first path (re-pattern (str atom-finder-corpus-path "/?")) ""))

(defn opname-or-typename
  "If the node is an expression, print out which kind, otherwise print out its type"
  [node]
  (-> node expr-operator :name (or (write-node-type node))))

;; List all node counts file-by-file
'((prn (now)))
'((->> atom-finder-corpus-path
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

;; Total node counts in entire corpus
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

(defn c-files-no-cpp-or-h
  "Search directory structure for C-only files"
  [dirname]
  (->> dirname
       files-in-dir
       (filter #(->> %1 .getName file-ext #{"c"} (and (.isFile %1))))))

;; All node counts line-by-line - creates a big file
'((->>
   "~/opt/src/atom-finder/gecko-dev"
   expand-home
   c-files-no-cpp-or-h
   (map parse-file)
   (mapcat (%->> find-all-atoms-non-atoms ((flip dissoc) :all-nodes) (mapcat (fn [[k vs]] (map #(vector % k) vs)))))
   (map (fn [[node atom-type]] (try
                                 {:file (filename node)
                                  :line (start-line node)
                                  :ast (-> node opname-or-typename name)
                                  :atom (-> atom-type name)}
          (catch Exception e nil))
          ))
   (remove nil?)
   (map (partial-right update-in [:file] #(.replaceAll % ".*gecko-dev/" "gecko-dev/")))
   (maps-to-csv "gecko-node-lines.csv")
   time-mins
   ))
