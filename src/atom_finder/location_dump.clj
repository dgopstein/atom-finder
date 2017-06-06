(ns atom-finder.location-dump
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.constants :refer :all]
   [atom-finder.classifier :refer :all]
   [clojure.pprint :refer [pprint]]
   [schema.core :as s]
   )
  (:import
   [org.eclipse.cdt.core.dom.ast IASTNode]
   )
  )

(defn loc-data [node]
  (select-keys (loc node) [:start-line :end-line :offset :length]))

(s/defn find-all :- [{:type s/Keyword s/Anyodes all-macro)
        non-atoms (set-difference-by :node
                                     all-nodes-macro
                                     all-atoms)]
    (map (partial merge {:file filename})
         (concat all-atoms (map #(merge {:type :non-atom} %1 (loc-data (:node %1))) non-atoms)))))


;(->> "/Users/dgopstein/opt/src/gcc/contrib/paranoia.cc" location-dump-atoms-and-non-atoms (map prn))
'(->> "/Users/dgopstein/opt/src/gcc/libatomic/gload.c" location-dump-atoms-and-non-atoms (map prn))

(defn now [] (java.util.Date.))
'(->> (now) println)

'(->> (str gcc-path "/libatomic")
     (pmap-dir-files location-dump-atoms-and-non-atoms)
     (mapcat (partial map #(update % :node write-node)))
     (map #(merge %1 (if (:path %1) {:depth (count (:path %1))} {})))
     (map (fn [m] (update m :file #(subs % (- (count gcc-path) 3))))) ; remove gcc-path prefix from file paths
     (map (juxt :file :type :start-line :end-line :depth))
     (map prn)
     (take 50000)
     dorun
     ;(log-to "location-dump_non-atoms_2017-06-05.txt")
     time
     )

(defn read-lines
  "read edn file with one entry per line"
  [filename]
  (->> filename
       slurp-lines
       (map read-string)
       ))

'(->> "tmp/location-dump_non-atoms_2017-06-05_1.txt"
     read-lines
     (take 1000000)
     (def location-dump-data)
     time
     )

(defn process-dump-file
  [[filename nodes-lst]]
  (let [nodes (map (fn [[type start-line end-line depth]]
                     {:type type :start-line start-line :end-line end-line :depth depth}) nodes-lst)
        {comments true non-comments false} (group-by #(= :comment (:type %)) nodes)
        all-line-items (->> nodes (filter :start-line) (group-by :start-line)) ; :start-line -> node
        ]
    (for [comment comments]
      (let [endl (:end-line comment)
            startl (:start-line comment)
            max-n 10
            line-items (->> all-line-items
                            (filter (fn [[line-num items]]
                                      (<= endl line-num (+ endl max-n))))
                            (into {})
                            ((fn [m] (update m startl (partial remove #{comment})))) ; remove this comment from consideration
                            ) ; limit the lines we look at to ones near-ish this comment
            type-count (fn [nodes] (->> nodes (map :type) frequencies))
            ;inline? (boolean (some #(= (:start-line %) (:start-line comment)) non-comments))
            same-line-items (line-items startl)
            next-line (->> line-items keys (filter #(< endl %)) min-of); next non-blank line after comment
            next-line-items (line-items next-line)
            next-N-lines-items (fn [n] (->> line-items
                                            (filter (fn [[line-num items]]
                                                      (>= (+ n endl) line-num)))
                                            (mapcat last)))
            ]
        {:file filename
         :comment comment
         :same-line (type-count same-line-items)
         :next-line (type-count next-line-items)
         :next-line-n next-line
         :next-5-lines (type-count (next-N-lines-items 5))
         :next-10-lines (type-count (next-N-lines-items 10))
         }
      ))))

'(->> location-dump-data
     (partition-by first)
     (map (fn [lst] [(first (first lst)) (map rest lst)]))
     (drop 240)(take 3)
     (map process-dump-file)
     pprint
     )

'(->> (str gcc-path "/libstdc++-v3/include/ext/mt_allocator.h")
     parse-file
     flatten-tree
     (map start-line)
     )
