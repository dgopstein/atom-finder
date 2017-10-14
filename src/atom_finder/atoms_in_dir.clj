(ns atom-finder.atoms-in-dir
  (:require [atom-finder.util :refer :all]
            [atom-finder.classifier :refer :all]
            [atom-finder.constants :refer :all]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            ))

(defn all-atoms-in-tree
  "Find all instances of multiple atoms under this node"
  [atoms node]
  (->>
   atoms
   (map #(vector
          (:name %1)
          ;(:finder %1)))
          ((:finder %1) node)))
   (into {})
   ))

(defn atoms-in-file
  [atoms filename]
  [filename
   (->> filename
        expand-home
        parse-file
        (all-atoms-in-tree atoms)
       (map-values (partial map (comp :line loc)))
        )])

(defn print-atoms-in-dir
  [dirname atoms]
  (->> dirname
       (pmap-dir-files #(atoms-in-file atoms %))
       (map prn)
       count
       println
       time-mins
       ))

; (print-atoms-in-dir (expand-home "~/opt/src/redis") (map atom-lookup [:preprocessor-in-statement :reversed-subscript]))
; (print-atoms-in-dir (expand-home "~/opt/src/linux") (map atom-lookup [:macro-operator-precedence]))

; (require '[atom-finder.classifier :refer :all])
