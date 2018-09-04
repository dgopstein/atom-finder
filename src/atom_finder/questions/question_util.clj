(ns atom-finder.questions.question-util
  (:require
   [atom-finder.constants :refer :all]
   [atom-finder.util :refer :all]
   [clj-cdt.clj-cdt :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as string]
   [clojure.data.csv :as csv]
   [clojure.java.io :as io]
   [schema.core :as s]
   [swiss.arrows :refer :all]
   )
  (:import
   [org.eclipse.cdt.core.dom.ast IASTProblem]
   )
  )

(defn atom-finder-relative-path
  "Remove the ~/opt/src/atom-finder/ from the pathname"
  [path]
  (string/replace-first path (re-pattern (str atom-finder-corpus-path "/?")) ""))

(def problem? (partial instance? IASTProblem))
(defn problem-rate
  "What fraction of this nodes lineage have parse problems"
  [root]
  (let [flat-tree (flatten-tree root)
        h      (->> flat-tree (group-by problem?) (map-values count))
        total  (count flat-tree)]
    (float (/ (get h true 0) total))))

(defn likely-c-file
  "Use heuristics to check whether or not the AST is likely from a C file"
  [root]
  (and (-> root flatten-tree count (> 5))
       (> 0.1 (problem-rate root))))

(defn pmap-dir-trees [f dirname] (pmap-dir-c-files (comp f parse-file) dirname))

(def c-files-in-dir (%->> clojure.java.io/file file-seq (filter c-file?)))

(defn csv-to-maps [filename]
  (with-open [reader (io/reader filename)]
    (let [[header & csv-data] (csv/read-csv reader)]
      (->> csv-data
           (map (%->> (map vector header) (into {})))
           doall))))
