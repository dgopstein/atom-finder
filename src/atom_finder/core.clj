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
(->> mem-data
     flatten-res
     (def flat-data))
(->> flat-data
     (map #(select-keys % [:atom :comments-added-near-atoms :atom-count-after :comments-added-away-atoms  :ast-size-after]))
     (map vals)
     (def comment-counts)
     time
     )

(defn agg-comment-counts
  [comment-counts]
  ;; ((:preprocessor-in-statement 0 0 0 158) (:logic-as-control-flow 0 0 0 158))
(->> comment-counts
     (remove nil?)
     (group-by first); pprint)
     (map-values
      (partial map (partial rest))); pprint)
     (map-values-kv
      (fn [key val] (->> val
     transpose
     (map (partial apply +))
     ((fn [[a b c d]]
        [[a b c (int (/ d (count atom-lookup)))]
         (map #(format "%5f" (float %)) [(safe-div a b) (/ c d)])]))
     ;pr-str (str key ": ")
     ;println
     ;time
     )))))

;; Show a couple removed comments
(->> flat-data
     (filter (fn [row] (and
                        (#{:omitted-curly-braces :logic-as-control-flow :conditional}
                         (:atom row))
                        (< 0 (:comments-added-near-atoms row))
                        )))
     shuffle
     (take 200)
     (map #(select-keys % [:atom :revstr :file]))
     (group-by :atom)
     (map-values (partial take 10))
     vals
     (map (partial map vals))
     ;(sort-by str)
     (def comments-files)
     )

(defn github-link [rev-str file line]
  (str "https://github.com/gcc-mirror/gcc/blob/" rev-str "/" file "#L" line))

(let [added-comments
      (->> comments-files
           (mapcat identity)
           ;(map (partial map prn))
           (mapcat
            (fn [[atom rev-str file]]
              (let [srcs (atom-finder.atom-patch/before-after-data
                          gcc-repo (find-rev-commit gcc-repo rev-str) file)]
                (for [cmnt (atom-finder.comment-change/comments-added srcs)]
                  [atom rev-str file srcs cmnt]
                  )))))]
  (->> added-comments
       (map (fn [[atom rev-str file srcs cmnt]]
         (let [atoms ((:finder (atom atom-lookup)) (:ast-after srcs))
               acs (atom-finder.comment-change/atom-comments atoms [cmnt])
               line (:line (loc cmnt))]
           [atom (count acs) (github-link rev-str file line)]
           )
         ))
     ;first
     ;(map-values class)
     (map println)
       )
  )

;; count comments on a per-extension basis
(->> flat-data
     (filter :file)
     (group-by (comp file-ext :file))
     (map-values (partial map #(select-keys % [:atom :comments-added-near-atoms :atom-count-after :comments-added-away-atoms  :ast-size-after])))
     (map-values (partial map vals))
     (map-values agg-comment-counts)
     (filter (fn [[k {[[a b c d] rats] :omitted-curly-braces}]] (< 200 c)))
     pprint
     )

(def srcs (atom-finder.atom-patch/before-after-data gcc-repo (find-rev-commit gcc-repo "519d8cb612e9d641d133e4f65b4a48c3ef963f43") "gcc/lra-constraints.c"))

(added-comments-context srcs (atom-lookup :omitted-curly-braces))

(group-by even? [1 2 3])
(->>
 (parse-source "int f() { return 1; }")
 (get-in-tree [0])
 )
