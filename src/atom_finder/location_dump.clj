(ns atom-finder.location-dump
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.constants :refer :all]
   [atom-finder.classifier :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.set :as set]
   [schema.core :as s]
   [clojure.data.csv :as csv]
   [clojure.string :as str]
   )
  (:import
   [org.eclipse.cdt.core.dom.ast IASTNode IASTFunctionDefinition]
   )
  )

(defn loc-data [node]
  (select-keys (loc node) [:start-line :end-line :offset :length]))

(def atoms-and-comments (conj atoms {:name :comment :finder all-comments}))
(def atoms-and-comments-and-functions (conj atoms-and-comments {:name :function :finder (default-finder (partial instance? IASTFunctionDefinition))}))

(s/defn find-all :- [{:type s/Keyword s/Any s/Any}]
  "Apply every classifier to this node"
  ([node :- IASTNode] (find-all atoms-and-comments-and-functions node))
  ([atoms-and node :- IASTNode]
   (let [contexts (context-map node)]
     (->> atoms-and
          (mapcat
           (fn [atom-map]
             (for [atom ((:finder atom-map) node)]
               (merge {:type (:name atom-map) :node atom}
                      (loc-data atom)
                      (contexts atom)
                      )))))
     ))
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

(->> (now) println)

'(->> gcc-path
     (pmap-dir-files location-dump-atoms-and-non-atoms)
     (mapcat (partial map #(update % :node write-node)))
     (map #(merge %1 (if (:path %1) {:depth (count (:path %1))} {})))
     (map (fn [m] (update m :file #(subs % (- (count gcc-path) 3))))) ; remove gcc-path prefix from file paths
     (map (juxt :file :type :start-line :end-line :depth))
     (map prn)
     ;(take 50)
     dorun
     ;(log-to "tmp/location-dump_non-atoms_2017-06-21_1.txt")
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
     ;(take 10)
     (def location-dump-data)
     time
     )

(defn process-dump-file
  [type [filename nodes-lst]]
  (let [nodes (map (fn [[type start-line end-line depth]]
                     {:type type :start-line start-line :end-line end-line :depth depth}) nodes-lst)
        {comments true non-comments false} (group-by #(= type (:type %)) nodes)
        all-line-items (->> nodes (filter :start-line) (group-by :start-line)) ; :start-line -> node
        ]
    (->>
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
          {;:file filename
           ;:comment comment
           :same-line (type-count same-line-items)
           :next-line (type-count next-line-items)
           ;:next-line-n next-line
           :next-5-lines (type-count (next-N-lines-items 5))
           :next-10-lines (type-count (next-N-lines-items 10))
           }
          ))
      (apply merge-with (partial merge-with +))
      (merge {:file filename})
      )))

(s/defn process-all
  [files-lines]; :- [[(s/one String "filename") [[]]]]]
  (for [selector (conj (map :name atoms) :comment)]
    (->> (for [file-lines files-lines]
               (dissoc (process-dump-file selector file-lines) :file))
         (apply merge-with (partial merge-with +))
         merge-down
         (#(assoc % :type selector))
         )))

'(->> location-dump-data)
'(->> asymmetric-dump-data
     (partition-by first) ; group data by filenames
     (map (fn [lst] [(first (first lst)) (map rest lst)])) ; remove the redundancy of repeated filenames
     ;(take 20)
     ;(map prn))
     process-all
     (map prn)
     dorun
     ;(log-to "location-dump_comment-sums_2017-06-20_asymmetric.txt")
     time
     )

'(->> "tmp/location-dump_comment-sums_2017-06-11_1.txt"
     read-lines
     (def comment-sums)
     time
     )

(defn values-at [m keys]
  (map m keys))

(defn maps-to-csv [filename maps]
  (let [headers (keys (first maps))]
    (with-open [writer (clojure.java.io/writer filename)]
      (csv/write-csv writer (cons headers (map #(values-at % headers) maps))))))

(defn parse-comment-sum-header
  "Object titled :first-distance-second where first is the class of the first
   thing found, distance is how far apart we're looking and second is the type
   of the second thing found"
  [keywd]
  (let [[[whole fst dist snd]]
        (re-seq #":(.*)-(next-\d*|same)-?lines?-(.*)" (str keywd))]
    (when (nil? dist) (pprn [keywd whole fst dist snd]))
    [dist fst snd]
    ))

(def all-type-names (->> atoms (map :name) sort (concat [:comment :non-atom]) (map str) (map #(subs % 1))))

(defn comment-sums-to-proximity-grid-csv
  [comment-sums]

  (let [merged-sums (->> comment-sums (apply merge-with +))
        parsed-header (map-keys parse-comment-sum-header merged-sums)
        dists (->> parsed-header (group-by (comp first first)) (map-values (partial into {})))]
    (doseq [[dist vals] dists]
      (println dist)
      (println (str/join "," (cons "type" all-type-names)))
      (doseq [snd-type all-type-names]
        (->> all-type-names
             (map (fn [fst-type] (vals [dist fst-type snd-type])))
             (cons snd-type)
             (str/join ",")
             println
             )
        )))
  )

;; Comment distance by depth
;; Do comments happen at different frequenceies at different depths
;; Do AST nodes have comments on the same line, within 10 lines, etc, at different rates at different depths

'(->> location-dump-data
     ;(take 30000)
     (partition-by first) ; group data by filenames
     (map (fn [lst] [(first (first lst)) (map rest lst)])) ; remove the redundancy of repeated filenames
     (map last)
     ;(filter (fn [[_ s e _]] (and s e)))
     (pmap
      (fn [atom-positions]
        (let [comments  (filter (comp #{:comment} first) atom-positions)
              non-atoms (filter (fn [[t s e d]] (and (= :non-atom t) s e d)) atom-positions)
              comment-locs (set (mapcat (fn [[_ s e _]] (range s (inc e))) comments))]
          (reduce
           (fn [hash [type start end depth]]
             (merge-with (partial merge-with +) hash
                         {depth {
                          :same-line (bin (comment-locs start))
                          :1-line    (bin (not (empty? (set/intersection comment-locs (set (range (- start 1)  (inc start)))))))
                          :5-line    (bin (not (empty? (set/intersection comment-locs (set (range (- start 5)  (inc start)))))))
                          :10-line   (bin (not (empty? (set/intersection comment-locs (set (range (- start 10) (inc start)))))))
                          :non-atoms 1
                          }})
            ) {} non-atoms))))
     (reduce (partial merge-with (partial merge-with +)))
     sort
     (map (fn [[k v]] (str/join "," (cons k (vals v)))))
     (take 100) ; depth >100 isn't super relevant
     (map println)
     time
     )

;; find depth 8 nodes whose parents are on a different line in an 8-heavy file
'(->> "~/opt/src/gcc/libdecnumber/decCommon.c"
     expand-home
     parse-file
     ;print-tree
     (filter-tree #(and (= 9 (depth %1)) (not= (start-line %1) (start-line (parent %1)))))
     (map (juxt start-line write-ast))
     pprint     )

;; list of all node type by depth
'(->> gcc-path
     (pmap-dir-files location-dump-atoms-and-non-atoms)
     (take 1000)
     (mapcat (partial map #(update % :node write-node)))
     (map #(merge %1 (if (:path %1) {:depth (count (:path %1))} {})))
     (map (fn [m] (update m :file #(subs % (- (count gcc-path) 3))))) ; remove gcc-path prefix from file paths
     (group-by :depth)
     (map-values (partial map :node))
     ;(map prn)
     (map-values frequencies)
     (map-values #(sort-by (comp - last) %))
     (map-values #(take 20 %))
     (sort-by first)
     (take 20)
     pprint
     dorun
     time
     )

;; find depth 8 nodes with comments near them, but not their parents
'(->> gcc-path
     (pmap-dir-files location-dump-atoms-and-non-atoms)
     (mapcat identity) ;(partial map #(update % :node write-node)))
     (map #(merge %1 (if (:path %1) {:depth (count (:path %1))} {})))
     (map (fn [m] (update m :file #(subs % (- (count gcc-path) 3))))) ; remove gcc-path prefix from file paths
     (partition-by :file) ; group data by filenames
     (take 1000)
     (pmap
      (fn [atom-positions]
        (let [comments  (filter (comp #{:comment} :type) atom-positions)
              non-atoms (filter (fn [{t :type s :start-line e :end-line d :depth}] (and (= :non-atom t) s e d)) atom-positions)
              comment-locs (set (mapcat (fn [{s :start-line e :end-line}] (range s (inc e))) comments))]
          ;(pprn non-atoms)
          (->> non-atoms
               (filter
                (fn [hash]
                  (let [{type :type start :start-line end :end-line depth :depth node :node} hash]
                    (and (= 8 depth)
                         (comment-locs start)
                         (not (comment-locs (start-line (parent node)))))
                    )
                  ))
               (map (partial tap (comp print-node-context :node)))
               )
               )))
     flatten
     (take 10)
     (map prn)
     dorun
     time
     )
