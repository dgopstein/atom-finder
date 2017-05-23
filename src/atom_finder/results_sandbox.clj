(ns atom-finder.atom-stats
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.constants :refer :all]
   [atom-finder.comment-change :refer :all]
   [clojure.pprint :refer [pprint]]
   [schema.core :as s]
   )
  )

;(def mem-data-old mem-data)
;(def commen-aggs-old comment-aggs)
(comment
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
       ;(map-values-kv (fn [k v] (if (file-level-key? k) (long (/ v n-atoms)) v)))
       ))

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
     (group-by (comp :atom))
     (map-values stats-by-file-type)
     (def file-type-stats)
     time)

(do
(println (clojure.string/join "," (concat ["atom" ".ext"] (->> file-type-stats first val first val keys))))
(for [[atom exts] file-type-stats]
  (for [[ext data] exts]
    (println (clojure.string/join "," (concat [atom ext] (vals data)))))))


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

)
