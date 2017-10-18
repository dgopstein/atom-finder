(ns atom-finder.atom-stats
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.constants :refer :all]
   [atom-finder.classifier :refer :all]
   [atom-finder.atom-stats :refer :all]
   [atom-finder.atoms-in-dir :refer :all]
   [clojure.pprint :refer [pprint]]
   [schema.core :as s]
   )
  )

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

;; 2 mins?
'((->> ;"code-age_gcc_2017-10-14_02.edn" (pap (constantly (now))) read-data
     "src/data/code-age_gcc_2017-10-14_02.edn" read-lines
;     (take 100)
     (def code-age)
     time-mins))

'((->> code-age (filter (comp #{"554ee0ec5263001ccc070413ec96f9edc2298bee"
                                "fa89373fe15d5ba85c2a08d4d0d6915f260f045e"}
                              :rev-str)) (def big-change) time-mins))

'((->> big-change (group-by :path) (sort-by (fn [[k [v1 v2]]] (- (Math/abs (- (-> v1 :atoms :omitted-curly-braces (or 0)) (-> v2 :atoms :omitted-curly-braces (or 0))))))) (take 20) (map prn)))

'((->> "/gcc/testsuite/gcc.dg/c99-intconst-1.c" (str gcc-path) parse-file (def c99-intconst-1)))
'((->> c99-intconst-1 ((-> atom-lookup :omitted-curly-braces :finder)) (take 20) (def intconst-atoms)))
'((->> intconst-atoms first))
'((->> intconst-atoms last print-tree))

;; 30 secs | 14 mins all of gcc, lazily, 4 mins once loaded
'((->>
   code-age
   (filter :path)
   (remove #(->> % :path (re-find #"test\/|\/test")))
   (group-by (juxt :date :rev-str))
   (map-values #(->> % (map :atoms) (apply merge-with +)))
   (def atoms-by-month)
   time-mins
   ))

'((->>
   atoms-by-month
   (map-values-kv (fn [[date rev-str] v] (merge v {:date date :rev-str rev-str})))
   vals
   (filter :date)
   (sort-by :date)
   reverse
   (maps-to-csv "gcc-atoms-by-month-no-test.csv")
   time-mins
   ))
