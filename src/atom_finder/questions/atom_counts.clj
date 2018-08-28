;; For a single state (e.g. head, or a sable release) how many atoms
;; of each type exist in the codebase, relative to total AST nodes

(ns atom-finder.questions.atom-counts
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.constants :refer :all]
   [atom-finder.atom-patch :refer :all]
   [atom-finder.classifier :refer :all]
   [atom-finder.questions.question-util :refer :all]
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

(defn count-atoms-in-tree
  [root]
  (map-values count (find-all-atoms-non-atoms root)))

(defn count-atoms-in-files [filename]
  (->> "~/opt/src/atom-finder"
       ;(pap (constantly (now)))
       expand-home
       (pmap-dir-trees
        (fn [root]
          {:file (str/replace-first (.getFilePath root) #".*/atom-finder/" "")
           :atom-counts (count-atoms-in-tree root)}))
       (map prn)
       dorun
       (log-to filename)
       time-mins
       ))

(defn summarize-atom-counts-totals [filename]
  (->> filename
     read-lines
     (map :atom-counts)
     (reduce (partial merge-with +))
     pprint
     time-mins)
  )

(defn summarize-atom-counts-by-project [edn-file csv-file]
  (->> edn-file
     read-lines
     (remove nil?)
     (map #(assoc (:atom-counts %1) :project (-> %1 :file (str/split #"/") first)))
     (group-dissoc :project)
     (map-values (partial reduce (partial merge-with +)))
     (map (fn [[proj counts]] (assoc counts :project proj)))
     (sort-by (comp - :all-nodes))                        ;; order rows
     ((fn [h] (maps-to-csv csv-file                       ;; order cols
               {:headers (->> h first (remove (comp #{:project} first))
                              (sort-by (comp - second))
                              (map first) (cons :project))}
               h)))
     time-mins)
  )

;; Generate a full CSV of the data, useful for by-module or by-directory analysis
;; See src/analysis/directory-counts.R
(defn atom-counts-to-csv [edn-file csv-file]
  (->> edn-file
       read-lines
       ;rest
       (remove nil?)
       (map #(assoc (:atom-counts %1) :file (:file %1)))
       (maps-to-csv csv-file)
       pprint
       ))

(defn main-atom-counts []
  (let [edn-file "tmp/atom-counts_2018-08-27_filter-better-extensions.edn"
        csv-file "src/analysis/data/atom-counts_2018-08-27_filter-better-extensions.csv"]
    (count-atoms-in-files edn-file)
    (summarize-atom-counts-by-project edn-file csv-file)
    ))
