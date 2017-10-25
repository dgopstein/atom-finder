(ns atom-finder.atom-stats
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.constants :refer :all]
   [atom-finder.classifier :refer :all]
   [atom-finder.atom-stats :refer :all]
   [atom-finder.atoms-in-dir :refer :all]
   [clojure.pprint :refer [pprint]]
   [schema.core :as s]
   [swiss.arrows :refer :all]
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

'((->> code-age (filter (comp
                      ;; #{"554ee0ec5263001ccc070413ec96f9edc2298bee" "fa89373fe15d5ba85c2a08d4d0d6915f260f045e"} ;;2004-02-01 2004-01-01
                         #{"a5877c89dcff8a2345d8f296cb6226dd3ddfe4a9" "1b1da1eead76fa1d94b5e6decefc3eea0ce5e260"}  ;; 2009-03-01 2009-02-01
                              :rev-str)) (def big-change) time-mins))

'((-<>> big-change (group-by :path) (map (fn [[k [v1 v2]]] [k (safe-div (- (-> v1 :atoms :omitted-curly-braces (or 1)) (-> v2 :atoms :omitted-curly-braces (or 1))) (- (-> v1 :atoms :non-atoms (or 1)) (-> v2 :atoms :non-atoms (or 1)))) v1 v2])) (sort-by (comp - #(Math/abs %) double second)) (take 21) (map prn)))

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
