(ns atom-finder.core
  (:require [atom-finder.classifier :refer :all]
            [atom-finder.count-subtrees :refer :all]
            [atom-finder.constants :refer :all]
            [atom-finder.util :refer :all]
            [atom-finder.classifier-util :refer :all]
            [atom-finder.results-util :refer :all]
            [atom-finder.atoms-in-dir :refer :all]
            [atom-finder.atom-patch :as atom-patch]
            [clojure.pprint :refer [pprint]]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            ))

(defn -main
  [& args]

  ;(->> (atom-patch/atoms-changed-all-commits gcc-repo atoms)
  ;     ;(map prn)
  ;     (take 10)
  ;     dorun)

  ; 3:51
  (atom-patch/log-atoms-changed-all-commits "gcc-bugs-atoms_2017-03-20.txt" gcc-repo atoms)
)

;(->> "macro-operator-precedence_redis_2017-03-10_1.edn"
;     read-data
;     (take 30)
;     (found-atom-source :macro-operator-precedence)
;     ;sum-found-atoms
;     )

;sum-found-atoms => {:macro-operator-precedence 1760}
;sum-found-atoms => {:macro-operator-precedence  759} # After removing atomic macros (#define M1 123)

(atom-patch/atoms-changed-in-commit gcc-repo [(->> atom-lookup :omitted-curly-braces)] "bda4a41c0259cba62881181f08fc1e6fcc67d5f7")
