(ns atom-finder.patch
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.string :as string]
   ))

;; Parse a .diff/.patch file for the lines of changes

(defn context-lines
  "Which lines are contained in this patch"
  [patch]
  ; @@ -1 +1 @@
  ; @@ -298,9 +298,23 @@
  (let [regex     #"@@ -(\d+),?(\d*) \+(\d+),?(\d*) @@"
        captures  (->> patch (re-seq regex) (map rest))
        type-conv (map (partial map (fn [int-str]
                                      (case int-str
                                        "" 1
                                        (Integer/parseInt int-str)))) captures)
        ]
        type-conv
    ))

(defn removed-lines
  "Which lines are removed in this patch"
  [patch]
    
  )
