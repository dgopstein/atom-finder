;; Count how many files cannot be parsed by CDT

(ns atom-finder.questions.count-parse-failures
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.constants :refer :all]
   [clojure.pprint :refer [pprint]]
   [schema.core :as s]
   [swiss.arrows :refer :all]
   [clojure.string :as str]
   )
  (:import
   [org.eclipse.cdt.core.dom.ast IASTNode]
   )
  )

'((do
    (prn (now))
    (->>
     atom-finder-corpus-path
     ;;"~/opt/src/atom-finder/gcc/gcc/testsuite/" expand-home
     c-files
     (map #(->> % parse-file filename (log-err (.getName %) nil)))
     doall
     (def parsed-files)
     time-mins
     )


    (->> parsed-files (take 2))
    (->> parsed-files count prn)
    (->> parsed-files (remove nil?) count prn)
    (->> parsed-files (filter nil?) count prn)
    ))
