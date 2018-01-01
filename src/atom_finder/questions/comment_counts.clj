(ns atom-finder.questions.comment-counts
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.constants :refer :all]
   [atom-finder.classifier :refer :all]
;   [atom-finder.location-dump :refer :all]
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

(defn separate-by-function
  [records] ; {:type :omitted-curly-braces, :start-line 245, :end-line 246, :offset 10887, :length 104, :path [12 2 0]}

  (let [[functions others] (separate #(= :function (:type %)) records)
        in-function? (->> functions
                          (map (fn [m] [(:start m) (:end m)]))
                          fn-range-set-cc)]
    (separate #(in-function? (:start %)) others)))

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

(def function-offset-range-set (comp fn-range-set-cc function-offset-ranges))

(defn nodes-near-comments-by-function
  "find all AST nodes near comments and
   categorize them by whether their in a function"
  [root]
  (let [comment-proximity-lines 3 ; how far away from a comment is still "commented"
        comments (all-comments root)
        function-offset-set (function-offset-range-set root)
        in-function-offset? #(and (not (function-node? %1))
                                  (offset %1)
                                  (function-offset-set (offset %1)))
        all-nodes  (flatten-tree root)
        all-node-lines (set (mapcat (juxt start-line end-line) all-nodes))
        inline-comment? #(all-node-lines (start-line %)) ; does a comment appear on the same line as an AST node
        [fn-comments global-comments] (separate in-function-offset? comments)
        [fn-comment-line-set global-comment-line-set] (for [comments [fn-comments global-comments]]
                                                        (->> comments ; the lines this comment is likely talking about
                                                           (map (fn [cmnt]
                                                                  (if (inline-comment? cmnt)
                                                                    [(start-line cmnt) (+ (end-line cmnt) comment-proximity-lines)]
                                                                    [(start-line cmnt) (end-line cmnt)])))
                                                           fn-range-set-cc))
        ]
    (->> all-nodes
         (concat (all-preprocessor root))
         (filter offset)
         (map (fn [node]
                {node
                 {:in-function? (in-function-offset? node)
                  :comment ((if (in-function-offset? node) fn-comment-line-set  global-comment-line-set) (start-line node))
                  ;:line (start-line node)
                  }}))
         (into {}))
    ))

(defn atoms-by-comments&function
  "Classify every node by whether its an atom,
   inside a function, and described by a comment"
  [root]
  (let [fn-cmnts (nodes-near-comments-by-function root)
        all-atoms (->> root find-all-atoms (mapcat (fn [[atm nodes]] (map #(vector atm %) nodes))))
        atom-cmnts     (map (fn [[atom-name node]] (merge {:node node :atom atom-name} (fn-cmnts node))) all-atoms)
        non-atom-cmnts (map (fn [[node   fn-cmnt]] (merge {:node node :atom nil} fn-cmnt)) (apply dissoc fn-cmnts (map second all-atoms)))
        ]

    (concat atom-cmnts non-atom-cmnts)
    ))

;; count comments by atoms and function in all of gcc
'(->> gcc-path
      (pmap-dir-trees
       (juxt filename
             #(->> %
                   atoms-by-comments&function
                   frequencies
                   (sort-by prn-str))))
      (map prn)
      (take 10)
      dorun
      (log-to "tmp/comments-by-atom-function.txt")
      time-mins)

;; find examples of atoms outside functions
'(->> gcc-path
     (pmap-dir-trees (juxt filename atoms-by-comments&function))
     (mapcat (fn [[filename hashes]] (map #(assoc % :file filename) hashes)))
     (filter :atom)
     (remove :in-function?)
     (remove (comp #{:macro-operator-precedence :preprocessor-in-statement} :atom))
     (take 10)
     (map prn))

;; merge all results
'(->> "tmp/comments-by-atom-function.txt"
     read-lines
     (mapcat second)
     (map (partial apply array-map))
     (reduce (partial merge-with +))
     (sort-by prn-str)
     (map prn))

;; 18 hours
'((->> "~/opt/src/atom-finder"
       expand-home
       list-dirs
       (map str)
       (mapcat (partial pmap-dir-trees atoms-by-comments&function))
       flatten
       (map (juxt :comment :atom))
       ;(take 2000)
       frequencies
       (map prn)
       dorun
       ;(maps-to-csv "comment-counts_2017-12-30_1.csv")
       time-mins
   ))

(->>
 [
[[true :omitted-curly-braces] 45892]
[[nil :conditional] 273]
[[nil nil] 6]
[[true :operator-precedence] 23890]
[[false :reversed-subscript] 23]
[[true :literal-encoding] 733]
[[nil :assignment-as-value] 104]
[[nil :omitted-curly-braces] 568]
[[true :conditional] 5082]
[[false :type-conversion] 12099]
[[nil :comma-operator] 5528]
[[nil :operator-precedence] 537]
[[false :literal-encoding] 3127]
[[false :omitted-curly-braces] 1583210]
[[false :comma-operator] 64706]
[[true :macro-operator-precedence] 240]
[[true :comma-operator] 2129]
[[nil :pre-increment] 14]
[[true :repurposed-variable] 3907]
[[true :post-increment] 4942]
[[true nil] 11497007]
[[true :preprocessor-in-statement] 177]
[[false :preprocessor-in-statement] 11568]
[[true :pre-increment] 1099]
[[true :reversed-subscript] 28]
[[false :post-increment] 72713]
[[false :implicit-predicate] 264357]
[[false :conditional] 151645]
[[false :assignment-as-value] 91731]
[[true :logic-as-control-flow] 420]
[[false :pre-increment] 22596]
[[true :assignment-as-value] 3419]
[[nil :implicit-predicate] 12841]
[[false :operator-precedence] 992817]
[[true :type-conversion] 1754]
[[false :logic-as-control-flow] 9547]
[[false nil] 277808231]
[[false :macro-operator-precedence] 3768]
[[nil :post-increment] 10]
[[true :implicit-predicate] 7997]
[[false :repurposed-variable] 79250]
[[nil :type-conversion] 9]
]

 (map #(update-in % [0 0] boolean))
 (map (partial apply array-map))
 (apply merge-with +)
 (map (fn [[[comment atom] count]] {:comment comment :atom (when atom (name atom)) :count count}))
 (maps-to-csv "comment-summary_2017-12-31.csv")

 )
