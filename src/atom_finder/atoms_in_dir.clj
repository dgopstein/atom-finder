(ns atom-finder.atoms-in-dir
  (:require [atom-finder.util :refer :all]
            [atom-finder.classifier :refer :all]
            [atom-finder.constants :refer :all]
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

(defn print-atoms-in-dir
  "Find all preprocessor directives not at the top level in directory"
  [dirname atoms]
  (->> dirname
       (pmap-dir-files
        #(vector %1
               (->> %1
                    parse-file
                    (all-atoms-in-tree atoms)
                    (map-values (partial map (comp :line loc)))
                    )))
       (map prn)
       count
       println
       time
       ))

; (print-atoms-in-dir (expand-home "~/opt/src/redis") (map atom-lookup [:preprocessor-in-statement :reversed-subscript]))
