(ns atom-finder.function-dump
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.constants :refer :all]
   [atom-finder.classifier :refer :all]
   [atom-finder.location-dump :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.set :as set]
   [schema.core :as s]
   [clojure.data.csv :as csv]
   [clojure.string :as str]
   )
  (:import
   [org.eclipse.cdt.core.dom.ast IASTNode]
   )
  )

'(->>
 (parse-file "~/opt/src/gcc/libdecnumber/decContext.c")
 (find-all atoms-and-comments-and-functions)
 pprint
 )

(defn separate-by-function
  [records] ; {:type :omitted-curly-braces, :start-line 245, :end-line 246, :offset 10887, :length 104, :path [12 2 0]}

  (let [[functions others] (separate #(= :function (:type %)) records)
        in-function? (->> functions
                          (map (fn [m] [(:start m) (:end m)]))
                          range-set-cc)]
    (separate #(in-function? (:start %)) others)))

'(->> "tmp/location-dump_non-atoms_2017-07-10_1.txt"
     read-lines
     (filter (partial every? identity))
     (def function-dump-data)
     )

'(pprn (count function-dump-data))

'(->> function-dump-data
     (map second)
     frequencies)

'(->> function-dump-data
     ;(map pap)
     (partition-by first)
     (map (partial map (fn [[file type start end depth]] {:file file :type type :start start :end end})))
     (map #(try (separate-by-function %) (catch Exception e '(({:type :exception})({:type :exception})))))
     (reduce (fn [[a-true a-false] [b-true b-false]] [(concat a-true b-true) (concat a-false b-false)]))
     (map (partial map :type))
     (merge-with +)
     (map frequencies)
     pprint
     time-mins
     )

(defn not-in-function-by-node
  "A set of all the AST nodes not inside a function"
  [root]
  (if (function-node? root)
    #{}
    (apply clojure.set/union (conj (map not-in-function-by-node (children root)) #{root}))))

(defn function-nodes
  [node]
   (if (function-node? node)
     (list node)
     (mapcat function-nodes (children node))))

(def node-offset-range (juxt offset end-offset))

(defn node-range-set
  "create a cc range-set from a list of nodes"
  [nodes]
  (->> nodes
       (map node-offset-range)
       (filter (partial every? identity))))

(defn function-offset-ranges
  "the closed ranges of what function definitions live"
  [root]
  (node-range-set (function-nodes root)))

(def function-offset-range-set (comp range-set-cc function-offset-ranges))

(defn nodes-near-comments-by-function
  "find all AST nodes near comments and
   categorize them by whether their in a function"
  [root]
  (let [comments (all-comments root)
        function-offset-set (function-offset-range-set root)
        in-function-offset? #(and (not (function-node? %1))
                                  (offset %1)
                                  (function-offset-set (offset %1)))
        [fn-comments global-comments] (separate in-function-offset? comments)
        [fn-comment-line-set global-comment-line-set] (for [comments [fn-comments global-comments]]
                                                        (->> comments ; the lines this comment is likely talking about
                                                           (map (fn [cmnt]
                                                                  (if (.isBlockComment cmnt)
                                                                    [(start-line cmnt) (+ (end-line cmnt) 3)]
                                                                    [(start-line cmnt) (end-line cmnt)])))
                                                           range-set-cc))
        ]
    (->> root
         flatten-tree
         (concat (all-preprocessor root))
         (filter offset)
         (map (fn [node]
                {node
                 {:in-function? (in-function-offset? node)
                  :comment ((if (in-function-offset? node) fn-comment-line-set  global-comment-line-set) (start-line node))
                  }}))
         (into {}))
    ))

(defn atoms-by-comments&function
  "Classify every node by whether its an atom,
   inside a function, and described by a comment"
  [root]
  (let [fn-cmnts (nodes-near-comments-by-function root)
        all-atoms (->> root find-all-atoms (mapcat (fn [[atm nodes]] (map #(vector atm %) nodes))))
        atom-cmnts     (map (fn [[atom-name node]] (merge {:atom atom-name} (fn-cmnts node))) all-atoms)
        non-atom-cmnts (map (fn [[node   fn-cmnt]] (merge {:atom nil} fn-cmnt)) (apply dissoc fn-cmnts (map second all-atoms)))
        ]

    (concat atom-cmnts non-atom-cmnts)
    ))

;; count comments by atoms and function in all of gcc
(->> gcc-path
 (pmap-dir-trees
  (juxt filename
        #(->> %
              atoms-by-comments&function
              frequencies
              (sort-by prn-str))))
 (map prn)
 (take 3)
 dorun
 time-mins)

;; merge all results
'(->> "tmp/comments-by-atom-function.txt"
     read-lines
     (mapcat second)
     (map (partial apply array-map))
     (reduce (partial merge-with +))
     (sort-by prn-str)
     (map prn)
     )
