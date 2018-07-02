(ns atom-finder.find-atoms-in-dirs
  (:require
   [swiss.arrows :refer :all]
   [atom-finder.util :refer :all]
   [atom-finder.classifier :refer :all]
   [atom-finder.atoms-in-dir :refer :all])
  )

(defn -main [& args]

  (doseq [dir args]
    (-<>> dir
         expand-home
         clojure.java.io/file
         .getCanonicalPath
         (atoms-in-dir <> atoms)
         (map prn)
         dorun))

  (shutdown-agents))

