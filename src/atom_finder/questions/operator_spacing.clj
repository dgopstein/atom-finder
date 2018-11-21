;; From David M Jones: "Sometimes developers use spacing to indicate binary
;; operator precedence; closer for greater precedence. So x+y * z is
;; suspicious (this suspicious usage occurs surprisingly often).

;; How often are operands mis-spaced?

(ns atom-finder.questions.operator-spacing
  (:require
   [atom-finder.constants :refer :all]
   [atom-finder.tree-diff :refer :all]
   [atom-finder.util :refer :all]
   [atom-finder.questions.question-util :refer :all]
   [clj-cdt.clj-cdt :refer :all]
   [clj-cdt.modify-ast :refer :all]
   [clj-cdt.writer-util :refer :all]
   [clj-cdt.expr-operator :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as string]
   [swiss.arrows :refer :all]
   [schema.core :as s]
   )
  (:import
   [org.eclipse.cdt.core.dom.ast IASTNode]
   )
  )

(def clustered-ops-regex #"\w+(?:[*/%+-<>&^|&=*!]+\w+)+")
(def spaced-ops-regex #"\w+\s+(?:[*/%+-<>&^|&=*!]+\s+\w+)+")

(defmulti mixed-spacing? "Does a binary expression use inconsistent spacing?" class)
(defmethod mixed-spacing? IASTNode [node] (-> node .getRawSignature mixed-spacing?))
(defmethod mixed-spacing? String [node-str]
  (boolean (and (re-find clustered-ops-regex node-str)
                (re-find spaced-ops-regex node-str))))

(defmulti parse-by-spacing "Infer the predecedence rules intended by the author by their spacing" class)
(defmethod parse-by-spacing IASTNode [node] (-> node .getRawSignature parse-by-spacing))
(defmethod parse-by-spacing String [node-str]
  (->> node-str
       (re-seq clustered-ops-regex)
       (reduce (fn [whole-expr sub-expr]
                 (string/replace whole-expr sub-expr (str "(" sub-expr ")")))
               node-str)
       parse-expr))

(defn remove-all-parens [node]
  (let [new-kids (into [] (map remove-all-parens (children node)))
        new-mom (replace-all-exprs node new-kids)]
    (->> new-mom
         children
         (filter paren-node?)
         (reduce
          (fn [mom paren-child] (replace-expr mom paren-child (child paren-child)))
          new-mom))))

(defn operator-spacing-confusing? [node]
  (not (tree=by (juxt class expr-operator)
                (remove-all-parens node)
                (->> node .getRawSignature parse-by-spacing remove-all-parens))))

(defn binary-expr? [node]
  (boolean (some->> node expr-operator :arity (= 2))))

(defn count-all-spaced-operators
  [edn-file]
  (println (str (now)))
  (->> atom-finder-corpus-path
       (pmap-dir-c-files
        (fn [file]
          (let [binaries (->> file parse-file (filter-tree binary-expr?))]
            (assoc
             :mixed-spacing (->> binaries (filter mixed-spacing?) count)
             :confusing-spacing (->> binaries (filter operator-spacing-confusing?) count)
             :all-binary (->> binaries count)
             :file (atom-finder-relative-path file)))))
       (map prn)
       (take 10)
       dorun
       ;(log-to edn-file)
       time-mins
       ))

(defn main-operator-spacing
  []
  (let [edn-file "tmp/operator-spacing_2018-11-20.edn"
        csv-file "src/analysis/data/operator-spacing_2018-11-20.csv"
        ]
    (count-all-spaced-operators edn-file)
    ;(summarize-all-nodes edn-file csv-file)
  ))

(s/defn replace-exprs2 :- IASTNode
  "Update multiple children inside a parent node.
   Returns an updated copy of the parent."
  [parent :- IASTNode
   old-kids ;;:- [IASTExpression] - These don't play nicely with java arrays
   new-kids ;;:- [IASTExpression]
   ]

  (assert (=by count old-kids new-kids) (str "Old expressions must be replaced by the same number of new expressions. You tried to replace " (count old-kids) " with " (count new-kids)))

  (let [{getters :getters setters :setters} (expr-getters-setters parent)
        old-idxs (map (fn [old-child] (.indexOf (map #(%) getters) old-child)) old-kids)]

    (reduce (fn [mom [old-idx new-kid]]
              (let [setter (nth setters old-idx)]
                (setter new-kid))) parent (map vector old-idxs new-kids))))
