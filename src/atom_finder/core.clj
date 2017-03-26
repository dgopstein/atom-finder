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

  ; 4:10
  ;(atom-patch/log-atoms-changed-all-commits "gcc-bugs-atoms_2017-03-20.txt" gcc-repo atoms)

  ;(println "sleeping")
  ;(Thread/sleep 10)
  ;(println "running")
  ;(def old-commit-hash "151ad919455c7143abb03ba325d073e7f86523bc")
  ;(time (dotimes [n 1000] (parse-commit-for-atom gcc-repo atoms (find-rev-commit gcc-repo old-commit-hash))))
)

;(->> "macro-operator-precedence_redis_2017-03-10_1.edn"
;     read-data
;     (take 30)
;     (found-atom-source :macro-operator-precedence)
;     ;sum-found-atoms
;     )

;sum-found-atoms => {:macro-operator-precedence 1760}
;sum-found-atoms => {:macro-operator-precedence  759} # After removing atomic macros (#define M1 123)
