(ns atom-finder.core
  (:require [atom-finder.classifier :refer :all]
            [atom-finder.count-subtrees :refer :all]
            [atom-finder.constants :refer :all]
            [atom-finder.util :refer :all]
            [atom-finder.classifier-util :refer :all]
            [atom-finder.results-util :refer :all]
            [atom-finder.atoms-in-dir :refer :all]
            [atom-finder.atom-patch :refer :all]
            [atom-finder.source-versions :refer :all]
            [schema.core :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            ))

;(set! *warn-on-reflection* true)
(defn -main
  [& args]

  ;(->> (atom-patch/atoms-changed-all-commits gcc-repo atoms)
  ;     ;(map prn)
  ;     (take 10)
  ;     dorun)

  ; 48 hours
  ;(time (log-atoms-changed-all-commits "gcc-bugs-atoms_2017-05-11_0.edn" gcc-repo atoms))
)
