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
       (mapcat (fn [dir]
                 (let [proj (str/replace dir #".*\/" "")]
                   (->> dir
                    (pmap-dir-trees atoms-by-comments&function)
                    flatten
                    (map #(assoc % :proj proj))))))
       (map (juxt :proj :in-function? :comment :atom))
       (take 1000)
       frequencies
       (map prn)
       dorun
       (maps-to-csv "comment-counts_2018-01-08_01_in-function.csv")
       time-mins
   ))

'((->>
 [
[["mongo" true nil] 538153]
[["gcc" nil :operator-precedence] 62]
[["subversion" false :macro-operator-precedence] 19]
[["httpd" false :macro-operator-precedence] 48]
[["httpd" true :post-increment] 35]
[["clang" false :operator-precedence] 11576]
[["mysql-server" true :logic-as-control-flow] 39]
[["mongo" nil :operator-precedence] 30]
[["freebsd" false :logic-as-control-flow] 4054]
[["gcc" false :assignment-as-value] 5376]
[["emacs" nil :omitted-curly-braces] 13]
[["linux" false :type-conversion] 2185]
[["vim" false :operator-precedence] 2086]
[["clang" false :type-conversion] 753]
[["clang" true :type-conversion] 460]
[["linux" true :logic-as-control-flow] 63]
[["linux" true :operator-precedence] 7246]
[["subversion" true :conditional] 54]
[["clang" true :post-increment] 130]
[["gecko-dev" nil :omitted-curly-braces] 51]
[["mongo" true :conditional] 229]
[["gcc" true :assignment-as-value] 334]
[["gcc" nil :omitted-curly-braces] 138]
[["freebsd" false :comma-operator] 27431]
[["webkit" true :assignment-as-value] 30]
[["gecko-dev" nil :conditional] 77]
[["git" true :post-increment] 16]
[["mysql-server" true nil] 534013]
[["subversion" true :operator-precedence] 73]
[["linux" true :implicit-predicate] 2391]
[["webkit" false :operator-precedence] 33551]
[["freebsd" true :assignment-as-value] 1257]
[["mysql-server" false :assignment-as-value] 7520]
[["git" true :implicit-predicate] 32]
[["git" false :macro-operator-precedence] 27]
[["freebsd" false :operator-precedence] 219729]
[["nginx" false :omitted-curly-braces] 1]
[["gecko-dev" true nil] 1762118]
[["httpd" true :omitted-curly-braces] 89]
[["clang" false nil] 9534863]
[["webkit" false :omitted-curly-braces] 68842]
[["gecko-dev" true :logic-as-control-flow] 59]
[["git" false :implicit-predicate] 2750]
[["subversion" false :implicit-predicate] 386]
[["clang" nil :type-conversion] 1]
[["freebsd" nil :type-conversion] 4]
[["nginx" false nil] 721857]
[["vim" false nil] 595692]
[["vim" false :logic-as-control-flow] 82]
[["mongo" false :literal-encoding] 96]
[["mysql-server" true :literal-encoding] 3]
[["linux" false :post-increment] 20354]
[["emacs" false :type-conversion] 368]
[["linux" nil :post-increment] 2]
[["mysql-server" true :implicit-predicate] 226]
[["nginx" nil :comma-operator] 2]
[["emacs" nil :assignment-as-value] 1]
[["subversion" nil :comma-operator] 20]
[["vim" nil :conditional] 3]
[["clang" false :conditional] 4937]
[["gcc" false :omitted-curly-braces] 172948]
[["vim" true nil] 70797]
[["httpd" nil :operator-precedence] 8]
[["gcc" true :literal-encoding] 64]
[["emacs" false :operator-precedence] 4279]
[["gcc" false :type-conversion] 3824]
[["linux" nil :assignment-as-value] 33]
[["clang" true :operator-precedence] 828]
[["mongo" nil :comma-operator] 601]
[["vim" false :post-increment] 508]
[["gecko-dev" false :post-increment] 9433]
[["subversion" true :type-conversion] 1]
[["linux" nil :pre-increment] 5]
[["httpd" false :assignment-as-value] 1412]
[["nginx" true :operator-precedence] 29]
[["clang" nil :omitted-curly-braces] 19]
[["gcc" true :conditional] 838]
[["mongo" false :preprocessor-in-statement] 175]
[["subversion" true :assignment-as-value] 9]
[["mysql-server" true :conditional] 243]
[["mysql-server" false :preprocessor-in-statement] 220]
[["vim" true :post-increment] 91]
[["mysql-server" true :type-conversion] 50]
[["webkit" false :assignment-as-value] 1788]
[["clang" false :comma-operator] 1760]
[["webkit" true :logic-as-control-flow] 1]
[["mysql-server" true :operator-precedence] 1112]
[["mysql-server" nil :operator-precedence] 11]
[["httpd" true nil] 38751]
[["httpd" false :pre-increment] 124]
[["mysql-server" true :assignment-as-value] 401]
[["freebsd" false :type-conversion] 1849]
[["gecko-dev" false :comma-operator] 11428]
[["clang" false :pre-increment] 267]
[["gcc" true :comma-operator] 306]
[["mysql-server" false :type-conversion] 422]
[["gcc" true :preprocessor-in-statement] 10]
[["clang" nil :implicit-predicate] 19]
[["linux" true :conditional] 856]
[["mysql-server" true :omitted-curly-braces] 2145]
[["httpd" nil :implicit-predicate] 38]
[["gcc" false :pre-increment] 1746]
[["mysql-server" nil :pre-increment] 1]
[["freebsd" false :preprocessor-in-statement] 4528]
[["linux" false :literal-encoding] 1142]
[["gcc" false :preprocessor-in-statement] 1872]
[["webkit" true :literal-encoding] 11]
[["clang" false :assignment-as-value] 1167]
[["emacs" true :type-conversion] 6]
[["gcc" false :logic-as-control-flow] 952]
[["linux" true :repurposed-variable] 705]
[["git" false :repurposed-variable] 600]
[["gecko-dev" true :pre-increment] 148]
[["clang" nil :comma-operator] 7]
[["mongo" true :omitted-curly-braces] 4132]
[["mongo" false :macro-operator-precedence] 82]
[["clang" nil :assignment-as-value] 1]
[["freebsd" nil :omitted-curly-braces] 189]
[["freebsd" true :literal-encoding] 113]
[["gcc" true nil] 1077981]
[["httpd" false :implicit-predicate] 1225]
[["linux" nil :comma-operator] 1316]
[["mongo" false :assignment-as-value] 3961]
[["webkit" false :implicit-predicate] 3198]
[["webkit" false :repurposed-variable] 2799]
[["webkit" true :type-conversion] 16]
[["webkit" nil :operator-precedence] 14]
[["webkit" false :type-conversion] 613]
[["mongo" true :logic-as-control-flow] 48]
[["mysql-server" false :logic-as-control-flow] 684]
[["gecko-dev" false :type-conversion] 1784]
[["freebsd" false nil] 73872133]
[["emacs" nil :implicit-predicate] 143]
[["emacs" false nil] 1315213]
[["gecko-dev" true :macro-operator-precedence] 99]
[["gecko-dev" false :conditional] 22362]
[["gecko-dev" false :literal-encoding] 336]
[["mongo" true :preprocessor-in-statement] 11]
[["clang" true :macro-operator-precedence] 7]
[["nginx" true nil] 10951]
[["freebsd" true :type-conversion] 119]
[["nginx" false :conditional] 303]
[["freebsd" false :implicit-predicate] 64049]
[["nginx" false :repurposed-variable] 148]
[["mongo" true :implicit-predicate] 137]
[["gcc" true :reversed-subscript] 21]
[["webkit" false :literal-encoding] 87]
[["gecko-dev" nil :assignment-as-value] 14]
[["mongo" false :repurposed-variable] 2570]
[["webkit" true :conditional] 136]
[["mysql-server" true :repurposed-variable] 162]
[["httpd" true :conditional] 15]
[["freebsd" nil :comma-operator] 1651]
[["git" false :conditional] 1204]
[["vim" true :repurposed-variable] 73]
[["clang" true :omitted-curly-braces] 2257]
[["httpd" true :assignment-as-value] 43]
[["httpd" true :operator-precedence] 101]
[["subversion" false :assignment-as-value] 456]
[["httpd" nil :omitted-curly-braces] 1]
[["git" false :omitted-curly-braces] 15205]
[["linux" false :operator-precedence] 511708]
[["emacs" false :macro-operator-precedence] 1]
[["httpd" false :omitted-curly-braces] 2121]
[["freebsd" true :reversed-subscript] 2]
[["emacs" false :repurposed-variable] 959]
[["nginx" true :pre-increment] 3]
[["freebsd" true :post-increment] 1235]
[["clang" false :repurposed-variable] 1717]
[["mysql-server" true :macro-operator-precedence] 28]
[["nginx" false :pre-increment] 78]
[["nginx" false :assignment-as-value] 8]
[["gecko-dev" false :repurposed-variable] 10486]
[["gcc" true :omitted-curly-braces] 4702]
[["emacs" false :omitted-curly-braces] 12245]
[["mongo" false :post-increment] 2894]
[["gcc" false :macro-operator-precedence] 16]
[["vim" true :pre-increment] 19]
[["nginx" false :logic-as-control-flow] 6]
[["webkit" true :pre-increment] 42]
[["clang" nil :operator-precedence] 1]
[["vim" false :conditional] 571]
[["webkit" false :pre-increment] 1158]
[["mongo" false :comma-operator] 2768]
[["gecko-dev" false :pre-increment] 2638]
[["clang" true :implicit-predicate] 590]
[["gecko-dev" nil :operator-precedence] 99]
[["subversion" nil :conditional] 1]
[["emacs" false :literal-encoding] 3]
[["git" true nil] 27928]
[["mongo" false nil] 12196722]
[["gecko-dev" false :implicit-predicate] 11339]
[["webkit" true :implicit-predicate] 58]
[["subversion" false :operator-precedence] 4299]
[["mongo" true :operator-precedence] 966]
[["gcc" nil nil] 4]
[["gecko-dev" true :post-increment] 675]
[["subversion" nil :implicit-predicate] 57]
[["emacs" false :comma-operator] 609]
[["clang" false :macro-operator-precedence] 54]
[["clang" true :conditional] 727]
[["freebsd" nil :operator-precedence] 152]
[["gecko-dev" false :operator-precedence] 95871]
[["subversion" false :post-increment] 299]
[["git" nil :comma-operator] 4]
[["httpd" false :repurposed-variable] 117]
[["freebsd" true :repurposed-variable] 939]
[["freebsd" nil :conditional] 97]
[["git" true :assignment-as-value] 24]
[["gecko-dev" true :preprocessor-in-statement] 13]
[["gcc" true :operator-precedence] 2279]
[["clang" nil nil] 1]
[["emacs" true :omitted-curly-braces] 386]
[["mysql-server" false :implicit-predicate] 5481]
[["mongo" true :repurposed-variable] 227]
[["freebsd" false :repurposed-variable] 23331]
[["gcc" true :repurposed-variable] 720]
[["git" false :comma-operator] 182]
[["webkit" true :comma-operator] 31]
[["freebsd" true nil] 3223577]
[["linux" nil :omitted-curly-braces] 80]
[["gecko-dev" nil :comma-operator] 384]
[["httpd" false :post-increment] 547]
[["linux" nil :type-conversion] 1]
[["mongo" false :logic-as-control-flow] 434]
[["freebsd" true :pre-increment] 325]
[["gecko-dev" true :operator-precedence] 2902]
[["vim" true :assignment-as-value] 34]
[["gcc" false :repurposed-variable] 13523]
[["clang" false :omitted-curly-braces] 57280]
[["mysql-server" false :omitted-curly-braces] 36712]
[["clang" true :logic-as-control-flow] 28]
[["subversion" true :preprocessor-in-statement] 1]
[["subversion" false :repurposed-variable] 276]
[["gecko-dev" nil :implicit-predicate] 1025]
[["clang" false :reversed-subscript] 10]
[["linux" nil :operator-precedence] 156]
[["subversion" nil :omitted-curly-braces] 8]
[["mysql-server" false :pre-increment] 775]
[["httpd" false :operator-precedence] 2674]
[["mongo" true :assignment-as-value] 192]
[["git" nil :assignment-as-value] 1]
[["mongo" false :omitted-curly-braces] 42598]
[["nginx" false :comma-operator] 58]
[["webkit" true nil] 377950]
[["webkit" nil :conditional] 11]
[["linux" false :logic-as-control-flow] 1723]
[["vim" false :repurposed-variable] 269]
[["gecko-dev" true :repurposed-variable] 470]
[["httpd" nil :type-conversion] 1]
[["nginx" true :comma-operator] 5]
[["emacs" false :preprocessor-in-statement] 131]
[["mysql-server" true :pre-increment] 70]
[["clang" false :logic-as-control-flow] 65]
[["vim" nil :implicit-predicate] 6]
[["vim" false :pre-increment] 148]
[["mongo" false :operator-precedence] 18343]
[["nginx" false :post-increment] 633]
[["subversion" false :conditional] 1707]
[["vim" true :type-conversion] 3]
[["subversion" nil :operator-precedence] 1]
[["mysql-server" false :repurposed-variable] 1779]
[["mysql-server" false :post-increment] 3726]
[["gecko-dev" false :omitted-curly-braces] 110248]
[["webkit" true :macro-operator-precedence] 6]
[["httpd" true :pre-increment] 4]
[["webkit" false :post-increment] 2405]
[["httpd" false :logic-as-control-flow] 171]
[["subversion" false :pre-increment] 86]
[["gecko-dev" true :assignment-as-value] 315]
[["vim" nil :omitted-curly-braces] 4]
[["gcc" false nil] 32080162]
[["vim" true :implicit-predicate] 114]
[["linux" true :comma-operator] 161]
[["webkit" false nil] 14781487]
[["gcc" nil :assignment-as-value] 3]
[["vim" false :omitted-curly-braces] 6584]
[["freebsd" true :conditional] 1212]
[["subversion" true :omitted-curly-braces] 254]
[["emacs" nil :comma-operator] 2]
[["freebsd" nil :post-increment] 4]
[["clang" true :pre-increment] 145]
[["clang" false :implicit-predicate] 1405]
[["httpd" false :conditional] 1101]
[["mysql-server" true :post-increment] 261]
[["vim" true :preprocessor-in-statement] 18]
[["linux" true :literal-encoding] 397]
[["gcc" nil :implicit-predicate] 1407]
[["subversion" false nil] 2658157]
[["vim" true :omitted-curly-braces] 903]
[["linux" true :preprocessor-in-statement] 13]
[["linux" false :omitted-curly-braces] 549487]
[["subversion" false :comma-operator] 89]
[["emacs" false :conditional] 1481]
[["freebsd" false :omitted-curly-braces] 494893]
[["subversion" true :implicit-predicate] 14]
[["freebsd" false :pre-increment] 7622]
[["git" true :comma-operator] 4]
[["nginx" true :repurposed-variable] 2]
[["emacs" true :operator-precedence] 157]
[["linux" true :post-increment] 1720]
[["emacs" true :logic-as-control-flow] 21]
[["linux" false :conditional] 42039]
[["clang" false :literal-encoding] 16]
[["gcc" true :pre-increment] 82]
[["subversion" true nil] 52210]
[["nginx" true :conditional] 2]
[["linux" true :type-conversion] 324]
[["freebsd" true :preprocessor-in-statement] 94]
[["subversion" true :post-increment] 17]
[["linux" false :implicit-predicate] 130192]
[["mongo" false :conditional] 5679]
[["linux" nil :conditional] 54]
[["vim" false :implicit-predicate] 963]
[["gecko-dev" false :preprocessor-in-statement] 1284]
[["emacs" true :repurposed-variable] 25]
[["mongo" nil :implicit-predicate] 224]
[["emacs" false :post-increment] 741]
[["mysql-server" nil :conditional] 2]
[["linux" true nil] 3138913]
[["freebsd" nil :pre-increment] 5]
[["clang" false :post-increment] 606]
[["gecko-dev" true :omitted-curly-braces] 5129]
[["gecko-dev" true :comma-operator] 270]
[["freebsd" nil :assignment-as-value] 45]
[["httpd" false nil] 991826]
[["subversion" false :logic-as-control-flow] 24]
[["mongo" nil :omitted-curly-braces] 22]
[["nginx" true :assignment-as-value] 4]
[["subversion" true :pre-increment] 1]
[["mongo" true :post-increment] 237]
[["linux" true :pre-increment] 175]
[["mysql-server" false :conditional] 6150]
[["gcc" false :literal-encoding] 979]
[["vim" false :type-conversion] 5]
[["linux" false :pre-increment] 5211]
[["gcc" nil :conditional] 17]
[["nginx" true :post-increment] 8]
[["git" true :operator-precedence] 138]
[["emacs" true :pre-increment] 11]
[["mysql-server" true :comma-operator] 130]
[["vim" true :conditional] 31]
[["httpd" true :logic-as-control-flow] 3]
[["httpd" true :comma-operator] 7]
[["mysql-server" false :literal-encoding] 13]
[["httpd" false :type-conversion] 26]
[["vim" false :comma-operator] 41]
[["vim" true :logic-as-control-flow] 3]
[["git" false :operator-precedence] 6348]
[["emacs" true :conditional] 24]
[["freebsd" true :operator-precedence] 7183]
[["mysql-server" false :comma-operator] 4491]
[["gcc" false :post-increment] 8406]
[["emacs" true :post-increment] 47]
[["nginx" false :implicit-predicate] 50]
[["httpd" false :preprocessor-in-statement] 29]
[["mysql-server" nil :assignment-as-value] 6]
[["gecko-dev" true :literal-encoding] 47]
[["git" nil :conditional] 1]
[["linux" false :macro-operator-precedence] 414]
[["mysql-server" nil :implicit-predicate] 339]
[["git" true :type-conversion] 1]
[["gcc" false :comma-operator] 4485]
[["freebsd" true :logic-as-control-flow] 129]
[["gcc" false :implicit-predicate] 40272]
[["freebsd" false :assignment-as-value] 40837]
[["linux" false nil] 76927576]
[["vim" false :preprocessor-in-statement] 123]
[["gcc" false :conditional] 14497]
[["httpd" true :repurposed-variable] 3]
[["freebsd" true :omitted-curly-braces] 14272]
[["gcc" false :operator-precedence] 57360]
[["mysql-server" false nil] 9280437]
[["git" true :conditional] 5]
[["httpd" true :type-conversion] 6]
[["mysql-server" true :preprocessor-in-statement] 10]
[["linux" false :preprocessor-in-statement] 2228]
[["linux" nil :implicit-predicate] 4768]
[["gecko-dev" nil :pre-increment] 2]
[["gcc" true :post-increment] 388]
[["clang" true :literal-encoding] 2]
[["mongo" false :implicit-predicate] 1601]
[["webkit" false :preprocessor-in-statement] 239]
[["gcc" true :logic-as-control-flow] 18]
[["clang" true :assignment-as-value] 219]
[["emacs" true :assignment-as-value] 32]
[["vim" nil :operator-precedence] 3]
[["mysql-server" nil :comma-operator] 60]
[["freebsd" true :macro-operator-precedence] 9]
[["git" false :pre-increment] 209]
[["webkit" false :conditional] 9431]
[["mongo" true :pre-increment] 68]
[["clang" true nil] 602826]
[["git" true :omitted-curly-braces] 332]
[["clang" true :repurposed-variable] 480]
[["webkit" false :logic-as-control-flow] 133]
[["linux" false :assignment-as-value] 16448]
[["httpd" nil :comma-operator] 17]
[["git" nil :implicit-predicate] 37]
[["mongo" true :type-conversion] 25]
[["git" false :type-conversion] 7]
[["httpd" true :preprocessor-in-statement] 1]
[["clang" false :preprocessor-in-statement] 659]
[["mongo" true :comma-operator] 112]
[["subversion" false :type-conversion] 6]
[["mongo" true :literal-encoding] 96]
[["webkit" nil :comma-operator] 1290]
[["git" false nil] 1158478]
[["freebsd" nil :implicit-predicate] 4432]
[["linux" true :omitted-curly-braces] 10415]
[["webkit" true :post-increment] 82]
[["mongo" false :pre-increment] 2340]
[["freebsd" false :post-increment] 21425]
[["httpd" false :comma-operator] 123]
[["emacs" false :implicit-predicate] 1446]
[["clang" true :reversed-subscript] 5]
[["subversion" false :omitted-curly-braces] 14046]
[["webkit" true :operator-precedence] 676]
[["git" true :pre-increment] 6]
[["gecko-dev" nil nil] 1]
[["subversion" true :repurposed-variable] 11]
[["git" true :logic-as-control-flow] 7]
[["linux" true :assignment-as-value] 525]
[["vim" false :reversed-subscript] 2]
[["webkit" true :repurposed-variable] 78]
[["emacs" false :assignment-as-value] 933]
[["gcc" nil :type-conversion] 2]
[["gcc" true :type-conversion] 524]
[["mongo" false :type-conversion] 257]
[["emacs" false :pre-increment] 194]
[["gcc" true :implicit-predicate] 1913]
[["linux" false :repurposed-variable] 20676]
[["nginx" false :operator-precedence] 3390]
[["vim" nil :comma-operator] 2]
[["git" true :repurposed-variable] 12]
[["emacs" false :logic-as-control-flow] 276]
[["emacs" true nil] 40839]
[["mysql-server" false :operator-precedence] 21603]
[["vim" true :operator-precedence] 200]
[["clang" true :comma-operator] 202]
[["gecko-dev" true :type-conversion] 219]
[["webkit" false :comma-operator] 3757]
[["git" false :post-increment] 736]
[["gecko-dev" false :macro-operator-precedence] 1034]
[["emacs" true :comma-operator] 22]
[["mongo" nil :conditional] 3]
[["subversion" false :preprocessor-in-statement] 67]
[["mongo" nil :pre-increment] 1]
[["clang" true :preprocessor-in-statement] 6]
[["webkit" nil :omitted-curly-braces] 22]
[["webkit" nil :implicit-predicate] 346]
[["linux" true :macro-operator-precedence] 80]
[["gcc" false :reversed-subscript] 11]
[["gcc" true :macro-operator-precedence] 3]
[["mysql-server" nil :omitted-curly-braces] 21]
[["freebsd" false :conditional] 40183]
[["mongo" nil :post-increment] 4]
[["git" false :assignment-as-value] 688]
[["httpd" true :macro-operator-precedence] 3]
[["gecko-dev" false nil] 41693628]
[["nginx" true :logic-as-control-flow] 1]
[["gecko-dev" true :conditional] 710]
[["freebsd" false :literal-encoding] 455]
[["freebsd" true :implicit-predicate] 1979]
[["gcc" nil :comma-operator] 172]
[["git" false :logic-as-control-flow] 97]
[["freebsd" true :comma-operator] 879]
[["gecko-dev" true :implicit-predicate] 423]
[["webkit" false :macro-operator-precedence] 356]
[["gecko-dev" false :logic-as-control-flow] 846]
[["clang" nil :conditional] 7]
[["mongo" true :macro-operator-precedence] 5]
[["webkit" true :omitted-curly-braces] 876]
[["git" false :preprocessor-in-statement] 13]
[["freebsd" false :macro-operator-precedence] 1350]
[["httpd" true :implicit-predicate] 56]
[["mysql-server" false :macro-operator-precedence] 367]
[["emacs" true :implicit-predicate] 64]
[["linux" false :comma-operator] 7484]
[["vim" false :assignment-as-value] 433]
[["gecko-dev" false :assignment-as-value] 10704]
]

 (map #(update-in % [0 1] boolean))
 (map (partial apply array-map))
 (apply merge-with +)
 (map (fn [[[project comment atom] count]] {:project project :comment comment :atom (when atom (name atom)) :count count}))
 (maps-to-csv "comment-summary-projects_2018-01-02.csv")

 ))
