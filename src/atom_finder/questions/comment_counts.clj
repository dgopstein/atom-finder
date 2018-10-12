(ns atom-finder.questions.comment-counts
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.constants :refer :all]
   [atom-finder.classifier :refer :all]
;   [atom-finder.location-dump :refer :all]
   [atom-finder.questions.question-util :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.set :as set]
   [schema.core :as s]
   [clojure.data.csv :as csv]
   [clojure.string :as str]
   [clj-cdt.clj-cdt :refer :all]
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
  (let [comment-proximity-lines 1 ; how far away from a comment is still "commented"
        comments (all-comments root)
        function-offset-set (function-offset-range-set root)
        in-function-offset? #(and (not (function-node? %1))
                                  (offset %1)
                                  (function-offset-set (offset %1)))
        all-nodes  (potential-atom-nodes root)
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
  (let [file (filename root)
        fn-cmnts (nodes-near-comments-by-function root)
        all-atoms (->> root find-all-atoms (mapcat (fn [[atm nodes]] (map #(vector atm %) nodes))))
        atom-cmnts     (map (fn [[atom-name node]] (merge {:file file :node node :atom atom-name} (fn-cmnts node))) all-atoms)
        non-atom-cmnts (map (fn [[node   fn-cmnt]] (merge {:file file :node node :atom nil} fn-cmnt)) (apply dissoc fn-cmnts (map second all-atoms)))
        ]

    (concat atom-cmnts non-atom-cmnts)
    ))

;; 33 hours
(defn comment-counts-wrt-atoms [edn-file]
  (println (str (now)))
  (->> "~/opt/src/atom-finder"
       expand-home
       (pmap-dir-trees #(with-timeout 300 (atoms-by-comments&function %)))
       ;;(take 2)
       (mapcat (fn [file-nodes]
                 (->> file-nodes
                      (map #(assoc % :file (->> % :file atom-finder-relative-path)))
                      (map (partial-right dissoc :node))
                      frequencies
                      )))
       (map prn)
       dorun
       (log-to edn-file)
       time-mins
   ))

(defn proj-from-file [filename]
  (->> filename (re-find #"[^/]*")))

(defn summarize-comment-counts [edn-file csv-file]
  (->> edn-file
      ;; "tmp/comment-counts_2018-10-08_01_filter-with-timeout.edn"
       read-lines
       ;;(take 20)
       (map (fn [[{:keys [file in-function? comment atom]} count]]
              {:proj (proj-from-file file) :in-function (boolean in-function?)
               :comment (boolean comment) :atom (some-> atom name) :count count}))
       (group-by #(select-keys % [:proj :in-function :comment :atom]))
       (map (fn [[keys vals]] (assoc keys :count (->> vals (map :count) sum))))
       (maps-to-csv csv-file)
       time-mins
       ))

;; Find only literal-encoding in mongo
'((->>"~/opt/src/atom-finder/"
       expand-home
       (pmap-dir-trees atoms-by-comments&function)
       flatten
       (filter #(and (:atom %) (true? (:comment %))))
       (map (fn [h]
              (let [[no-node {node :node}] (split-map-by-keys h [:node])]
                (merge no-node {:line (start-line node) :file (filename node) :node (write-tree node)}))))
       (map prn)
       ;(take 20)
       dorun
       (log-to "tmp/comment-counts_2018-01-26_01_proximity-1-line.edn")
       time-mins
   ))

(defn main-comment-counts []
  (let [edn-file "tmp/comment-counts_2018-10-08_01_filter-with-timeout.edn"
        csv-file "src/analysis/data/comment-counts_2018-10-11_01_group-results.csv"]
    ;(comment-counts-wrt-atoms edn-file)
    (summarize-comment-counts edn-file csv-file)
    ))
