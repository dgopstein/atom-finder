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

(def mem-data-old mem-data)
(def commen-aggs-old comment-aggs)

(time (do
(->> "gcc-atom-comment-context_2017-05-17_1.edn"
     read-data
     (def mem-data) time)
(->> mem-data
     flatten-res
     (filter :ast-size-before) ; remove commits that didn't parse
     (def flat-data) time)
(->> flat-data
     (map #(dissoc % :atom :revstr :bug-ids :file))
     (def comment-counts)
     time
     )
(->> comment-counts
     ;pprint)
     (apply merge-with +)
     (def comment-aggs)
     time
     )

(pprint (sort comment-aggs))

(pprint (map-values #(format "%05f" (float %))
  {:inner-atom-comment-rate     (safe-div (:inner-comments-added-near-atoms comment-aggs)
                                          (:inner-atom-count comment-aggs))
   :inner-non-atom-comment-rate (safe-div (:inner-comments-added-away-atoms comment-aggs)
                                          (:inner-ast-size comment-aggs))
   :outer-atom-comment-rate (safe-div (:outer-comments-added-near-atoms comment-aggs)
                                      (:outer-atom-count comment-aggs))
   :outer-non-atom-comment-rate (safe-div (:outer-comments-added-away-atoms comment-aggs)
                                          (:outer-ast-size comment-aggs))
   }))

))

(defn file-level-key? [key] (not (re-find #"atom" (str key))))

(def keep-numeric-vals (partial dissoc-by (comp not number? last)))

(s/defn sum-atom-counts
  [n-atoms :- s/Int map-list]; :- [{:ast-size-before s/Int s/Any s/Any}]]
  (->> map-list
       (map keep-numeric-vals)
       (apply merge-with +)
       (map-values-kv
        (fn [k v]
          (if (file-level-key? k) (long (/ v n-atoms)) v)))))

(defn stats-by-file-type
  [flat-data]
  (let [n-atoms (->> flat-data (take 100) (map :atom) distinct (remove nil?) count)]
    (->> flat-data
         (group-by (comp file-ext :file))
         (map-values (partial sum-atom-counts n-atoms))
         ))
  )


(->> flat-data
     ;(filter #(= :omitted-curly-braces (:atom %)))
     stats-by-file-type
     (def file-type-stats)
     time)

(println (clojure.string/join "," (concat [".ext"] (keys (last (first file-type-stats))))))
(for [[k v] file-type-stats]
  (println (clojure.string/join "," (concat [k] (vals v)))))


(->> "~/opt/src/gcc/gcc/rtl.h"
     parse-file
     ((:finder (atom-lookup :omitted-curly-braces)))
     (map write-ast)
     (map (comp println println println))
     )

(->> ;"version.h"
     ;"ChangeLog"
 "mimetypes.default"
     parse-file
     print-tree)
     ;flatten-tree
     ;count)
