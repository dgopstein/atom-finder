;; find parens which are not technically necessary

(ns atom-finder.questions.superfluous-parens
  (:require
   [atom-finder.constants :refer :all]
   [atom-finder.classifier :refer :all]
   [atom-finder.util :refer :all]
   [atom-finder.tree-diff :refer :all]
   [atom-finder.questions.question-util :refer :all]
   [clj-cdt.clj-cdt :refer :all]
   [clj-cdt.expr-operator :refer :all]
   [clj-cdt.writer-util :refer :all]
   [clj-cdt.modify-ast :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as string]
   [swiss.arrows :refer :all]
   [schema.core :as s])
  (:import [atom_finder.classifier Atom])
   )

(s/defn superfluous-parens? [node :- (s/pred paren-node?)]
  (let [mom (parent node)
        kid (child node)
        parenless-mom (replace-expr mom node kid)
        reparsed-parenless-mom (->> parenless-mom write-ast parse-expr)
        ]

    (tree=by (juxt class expr-operator) parenless-mom reparsed-parenless-mom)
  ))

(defn macro-def-node?
  "Was this AST node parsed from inside the defintion of a macro?"
  [node]
  (->> node filename (= "anonymously-parsed-code.c")))

(defn find-parens
  [root]
  (let [paren-types
        [(Atom. :parens paren-node? (default-finder paren-node?))
         (Atom. :superfluous-parens superfluous-parens?
                (default-finder (fn [node]
                                  (when (and (-> node macro-def-node? not)
                                             (-> node parent misparsed-template? not)
                                             (-> node paren-node?))
                                   (superfluous-parens? node)))))
         (Atom. :multi-op-expr multi-op-expr? (default-finder multi-op-expr?))
         (Atom. :operator-precedence operator-precedence-child? (default-finder operator-precedence-child?))]]
  (find-atoms paren-types root)))

(defn child-or-nil
  "If this node has one child return it, otherwise return nil"
  [node]
  (let [[kid & others] (children node)]
    (when (empty? others)
      kid)))

(defn find-all-parens-in-project
  [edn-file]
  (println (str (now)))
  (->> atom-finder-corpus-path
       (pmap-dir-c-files
        (fn [file]
          (merge
           {:file (atom-finder-relative-path file)}
           (->> file parse-file find-parens
                (map-values (partial map (fn [node]
                                           (merge {:parent-type (-> node parent opname-or-typename)
                                                   :node-type (-> node opname-or-typename)
                                                   :child-type (some-> node child-or-nil opname-or-typename)}
                                                  (dissoc (loc node) :length :start-line)
                                                  )))))
           )))
       ;(take 1000)
       (map prn)
       dorun
       (log-to edn-file)
       time-mins
       ))

(s/defn flatten-all-parens
  [all-paren-lines] ; :- [{:file s/Str :parens s/Any :superfluous-parens s/Any :operator-precedence s/Any}]]
  (->> all-paren-lines
   ;(take 10)
   (mapcat (fn [file-map]
             (->> [:parens :superfluous-parens :operator-precedence]
                  (select-keys file-map)
                  (map-values-kv (fn [k v] (map #(assoc % :selection k) v)))
                  vals
                  (apply concat)
                  (map (partial into {}))
                  (map (fn [node-map]
                         (->> [:selection :parent-type :node-type :child-type]
                              (select-keys node-map)))))
          ))
  )
  )

(defn summarize-all-parens
  [edn-file csv-file]
  (->>
   edn-file
   read-lines
   ;(take 10)
   flatten-all-parens
   frequencies
   (map (partial-right update-in [0] (partial map-values #(some-> % name))))
   (map (fn [[m count]] (assoc m :count count)))
   ;(map prn)
   (maps-to-csv csv-file)
   time-mins
   )
  )

(defn multi-op-expr?
  "Return all nodes (recursively) that represent an expression which uses more
  than one operator"
  [root]
  (and (expr-operator root)
       (exists? expr-operator (children root))))

(defn --test-parse-file-for-superfluous-parens []
  "Just a place for sandbox code to live"
  (->> "sql/item_geofunc.cc"
     (str "~/opt/src/atom-finder/mysql-server/")
     expand-home
     parse-file
     ((default-finder #(when (paren-node? %) (superfluous-parens? %))))
     (take 4)
     last
     parent
     misparsed-template?
     prn
     ))

(defn main-superfluous-parens
  []
  (let [edn-file "tmp/all-parens_2018-10-30_02_multi-op-expr.edn"
        csv-file "src/analysis/data/all-parens_2018-10-30_02_multi-op-expr.csv"
        ]
    (find-all-parens-in-project edn-file)
    (summarize-all-parens edn-file csv-file)
  ))
