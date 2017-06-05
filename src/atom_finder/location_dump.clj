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

(s/defn find-all :- [{:type s/Keyword s/Any s/Any}]
  "Apply every classifier to this node"
  [node :- IASTNode]
  (let [contexts (context-map node)]
    (->> atoms
         ((flip conj) {:name :comment :finder all-comments})
         (mapcat
          (fn [atom-map]
            (for [atom ((:finder atom-map) node)]
              (merge {:type (:name atom-map) :node atom}
                     (loc-data atom)
                     (contexts atom)
                     ))))
         )
    )
  )

(s/defn set-difference-by
  "set-difference after applying a function to each element"
  [f s1 s2]
  (let [m1 (zipmap (map f s1) s1)]
    (vals (apply dissoc m1 (map f s2)))))
'(set-difference-by #(* %1 %1) [1 2 3] [-1 2 -4])

(defn location-dump-atoms [filename]
  (map (partial merge {:file filename}) (->> filename parse-file find-all)))

(defn location-dump-atoms-and-non-atoms [filename]
  (let [root      (->> filename parse-file)
        all-atoms (find-all root)
        all-nodes (->> root flatten-tree-context (map (fn [[ctx n]] (assoc ctx :node n))))
        all-macro (->> root all-preprocessor (map #(array-map :node %)))
        all-nodes-macro (concat all-nodes all-macro)
        non-atoms (set-difference-by :node
                                     all-nodes-macro
                                     all-atoms)]
    (map (partial merge {:file filename})
         (concat all-atoms (map #(merge {:type :non-atom} %1 (loc-data (:node %1))) non-atoms)))))


;(->> "/Users/dgopstein/opt/src/gcc/contrib/paranoia.cc" location-dump-atoms-and-non-atoms (map prn))

(defn now [] (java.util.Date.))
'(->> (now) println)

'(->> gcc-path
     (pmap-dir-files location-dump-atoms-and-non-atoms)
     (mapcat (partial map #(update % :node write-node)))
     (map #(merge %1 (if (:path %1) {:depth (count (:path %1))} {})))
     (map (fn [m] (update m :file #(subs % (- (count gcc-path) 3))))) ; remove gcc-path prefix from file paths
     (map (juxt :file :type :start-line :end-line :depth))
     (map prn)
     (take 50000)
     dorun
     (log-to "location-dump_non-atoms_2017-06-05.txt")
     time
     )

(defn read-lines
  "read edn file with one entry per line"
  [filename]
  (->> filename
       slurp-lines
       (map read-string)
       ))

(->> "tmp/location-dump_non-atoms_2017-06-02.txt_100000"
     read-lines
     (def location-dump-data)
     time
     )

(defn process-dump-file
  [[filename atoms-lst]]
  (let [atoms (map (fn [[atom start-line end-line depth]]
                     {:atom atom :start-line start-line :end-line end-line :depth depth}) atoms-lst)
        comments (filter #(= :comment (:atom %)) atoms)]
    (for [comment comments]
      (let [next-line (->> atoms (map :start-line) (filter #(<= (:end-line comment) %)) min-of) ; next non-blank line after comment
            next-line-items (filter #(= next-line (:start-line %)) atoms)
            next-line-counts (frequencies next-line-items)]
        (pprn [filename comment next-line next-line-counts])
        next-line-counts
      ))))

'(->> location-dump-data
     (take 50)
     (partition-by first)
     (map (fn [lst] [(first (first lst)) (map rest lst)]))
     (map process-dump-file)
     pprint
     )

(->> "/Users/dgopstein/opt/src/gcc/libatomic/gload.c"
     parse-file
     all-preprocessor
     (map loc)
     (map prn)
     )
