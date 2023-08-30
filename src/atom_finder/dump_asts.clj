(ns atom-finder.dump-asts
  (:require
   [swiss.arrows :refer :all]
   [clj-cdt.clj-cdt :refer :all]
   [clj-cdt.writer-util :refer [write-tree write-node]]
   [atom-finder.util :refer :all]
   [atom-finder.classifier :refer :all]
   [atom-finder.atoms-in-dir :refer :all])
  )

(defn -main [& args]
  (-<>> args ;; dirs
        (mapcat files-in-dir)
        (map normalize-path)
        (map parse-file)
        (mapcat flatten-tree)
        (remove nil?)
        (take 200)
        (map #(merge  {:filename (filename %)} (dissoc (loc %) :line) {:typename (typename %)} {:code (write-node %)}))
        maps-to-csv
       ))

