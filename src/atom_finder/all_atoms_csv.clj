(ns atom-finder.all_atoms_csv
  (:require
   [swiss.arrows :refer :all]
   [atom-finder.util :refer :all]
   [atom-finder.classifier :refer :all]
   [atom-finder.atoms-in-dir :refer :all]
   [clj-cdt.clj-cdt :refer :all]
   [clj-cdt.writer-util :refer :all]
  ))


(def path-separator java.io.File/separator)

(defn -main [& args]
  (println "atom,file,line,offset,code")
  (doseq [dir (map normalize-path args)]
    (let [dir-len (inc (clojure.string/last-index-of dir path-separator))]
      (-<>> dir
            (pmap-dir-c-files #(all-atoms-in-tree atoms (parse-file %)))
            (apply concat)
            (mapcat (fn [[k v]] (map #(vector k %) v)))
            (map (fn [[atom-type node]]
                   {:atom (name atom-type)
                    :file (filename node)
                    :line (start-line node)
                    :offset (offset node)
                    :code (write-tree node)}))
            (map (partial map-to-csv))
            (map print)
            dorun)))

  (shutdown-agents))


