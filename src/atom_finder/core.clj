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

  ; 48 hours
  ;(time (log-atoms-changed-all-commits "gcc-bugs-atoms_2017-05-11_0.edn" gcc-repo atoms))
)

;(->> "gcc-bugs-atoms_2017-05-11_0.edn"
;     read-data
;     (def mem-data)
;     )
;(->> mem-data
;     flatten-res
;     (map #(select-keys % [:comments-added-near-atoms :atom-count-after :comments-added-away-atoms  :ast-size-after]))
;     (map vals)
;     (def comment-counts)
;     time
;     )
;(->> comment-counts
;     (remove nil?)
;     transpose
;     (map (partial apply +))
;     ((fn [[a b c d]] [a b c (/ d (count atom-lookup))]))
;     ((fn [[a b c d]] [(/ a b) (/ c d)]))
;     (map #(format "%5f" (float %))) ; [commented-C commented-NC]
;     pprint
;     ;time
;     )

;sum-found-atoms => {:macro-operator-precedence 1760}
;sum-found-atoms => {:macro-operator-precedence  759} # After removing atomic macros (#define M1 123)

;(atom-patch/atoms-changed-in-commit gcc-repo [(->> atom-lookup :omitted-curly-braces)] "bda4a41c0259cba62881181f08fc1e6fcc67d5f7")

