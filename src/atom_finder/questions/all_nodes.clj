;; How often is each type of AST node used in our corpus

(ns atom-finder.questions.all-nodes
  (:require
   [atom-finder.constants :refer :all]
   [atom-finder.util :refer :all]
   [atom-finder.atom-patch :refer :all]
   [atom-finder.questions.question-util :refer :all]
   [clj-cdt.clj-cdt :refer :all]
   [clj-cdt.expr-operator :refer :all]
   [clj-cdt.writer-util :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as string]
   [swiss.arrows :refer :all]
   [schema.core :as s]
   )
  )

(defn opname-or-typename
  "If the node is an expression, print out which kind, otherwise print out its type"
  [node]
  (-> node expr-operator :name (or (write-node-type node))))

;; List all node counts file-by-file
(defn count-all-nodes-in-project
  [edn-file]
  (prn (now))
  (->> atom-finder-corpus-path
       (pmap-dir-c-files
        (fn [file]
          (assoc
           (->> file parse-file potential-atom-nodes (map opname-or-typename) frequencies)
           :file (atom-finder-relative-path file))))
       (map prn)
       dorun
       (log-to edn-file)
       time-mins
       ))

(defn c-files-no-cpp-or-h
  "Search directory structure for C-only files"
  [dirname]
  (->> dirname
       files-in-dir
       (filter #(->> %1 .getName file-ext #{"c"} (and (.isFile %1))))))

(defn summarize-all-nodes [edn-file csv-file]
  (->>
   edn-file
   read-lines
   (map (partial-right dissoc :file))
   (apply merge-with +)
   (sort-by (comp - last))
   (map (partial zipmap [:node-type :count]))
   (maps-to-csv csv-file)
   time-mins
   ))

;; some files are much better parsed as C,
;; but unfortunately our classifiers are not bilingual
'((-<>> "mysql-server/cmd-line-utils/libedit/el.c"
     (str atom-finder-corpus-path "/")
     slurp
     (parse-source <> {:language :c})
     (flatten-tree)
     (group-by problem?)
     (map-values count)
     pprint
     ))

(defn main-all-nodes
  []
  (let [edn-file "tmp/all-node-counts_2018-08-31_for-esem.edn"
        csv-file "src/analysis/data/all-node-counts_2018-08-31_for-esem.csv"]
    (count-all-nodes-in-project edn-file)
    (summarize-all-nodes edn-file csv-file)
  ))
