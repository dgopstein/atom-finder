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
   [org.eclipse.cdt.core.dom.ast IASTNode IASTBinaryExpression IASTIdExpression
    IASTElaboratedTypeSpecifier]
   )
  )

(defn re-join [& pats] (->> pats (map str) str re-pattern))

;;(def op-chars "[-*/%+>&^|&=*!]+")
(def op-chars "[-*/%+*]+")
(def word-chars "(?:\\w|->|\\.|\\(|\\))+")

(def clustered-ops-regex (re-pattern (str word-chars "(?:" op-chars word-chars ")+")))
(def spaced-ops-regex (re-pattern (str word-chars "\\s+(?:" op-chars "\\s+" word-chars ")+")))

;;(def op-exceptions #{"->" "."})

(defmulti mixed-spacing? "Does a binary expression use inconsistent spacing?" class)
(defmethod mixed-spacing? IASTNode [node] (-> node .getRawSignature mixed-spacing?))
(defmethod mixed-spacing? String [node-str]
  (boolean (and
    (->> node-str (re-seq clustered-ops-regex) not-empty?)
    (->> node-str (re-seq spaced-ops-regex) not-empty?))))

(defmulti parse-by-spacing "Infer the predecedence rules intended by the author by their spacing" class)
(defmethod parse-by-spacing IASTNode [node] (-> node .getRawSignature parse-by-spacing))
(defmethod parse-by-spacing String [node-str]
  (->> node-str
       (re-seq clustered-ops-regex)
       (remove #{node-str})
       (reduce (fn [whole-expr sub-expr]
                 (string/replace whole-expr sub-expr (str "(" sub-expr ")")))
               node-str)
       parse-expr))

(defn remove-all-parens [node]
  (let [old-kids (children node)
        new-kids (into [] (map remove-all-parens old-kids))
        new-mom (replace-all-exprs node new-kids)]

    (loop [mom new-mom]
      (let [paren-child (->> mom children (filter paren-node?) first)]
           (if (nil? paren-child)
             mom
             (recur (replace-expr mom paren-child (child paren-child))))))))


(def template-regex #"<\w+>") ;; CDT often misparses templates, so ignore them

(defn confusing-operator-spacing? [node]
  (let [node-str (.getRawSignature node)]
    (and (not (or (instance? IASTIdExpression node)
                  (instance? org.eclipse.cdt.internal.core.dom.parser.cpp.CPPASTQualifiedName node)
                  (re-find template-regex node-str)
                  (assignment? node)
                  ))

         (->> node expr-operator :precedence (< 4))

         (not (tree=by (juxt class expr-operator)
                       (remove-all-parens node)
                       (some->> node-str parse-by-spacing remove-all-parens))))))

(defn binary-expr? [node]
  (boolean (some->> node expr-operator :arity (= 2))))

(defn arithmetic-expr? [node]
  (->> node expr-operator :name
       (#{:miltiply :divide :modulo :plus2 :minus2}) boolean))

(defn count-all-spaced-operators
  [edn-file]
  (println (str (now)))
  (->> atom-finder-corpus-path
   expand-home
   (pmap-dir-c-files
    (fn [file]
      (let [binaries
            (->> file parse-file flatten-tree
                 (filter binary-expr?)
                 (filter arithmetic-expr?)
                 (remove (%->> flatten-tree (some (%->> expr-operator :name (#{:sizeof})))))
                 (remove (%->> flatten-tree (some (partial instance? IASTElaboratedTypeSpecifier))))
                 (remove (%->> flatten-tree (some contained-by-macro-exp?)))
                 (remove nil?))]
    ;(dorun (map (fn [node] (prn [(contained-by-macro-exp? (get-in-tree [0] node)) (write-tree node)])) binaries))
        {:mixed-spacing (->> binaries (filter mixed-spacing?) count)
         :confusing-spacing (->> binaries (filter confusing-operator-spacing?) (map (partial pap #(.getRawSignature %))) count)
         :all-binary (->> binaries count)
         :file (atom-finder-relative-path file)}))
    )
   ;(map prn)
   (take 500)
   dorun
   ;(log-to edn-file)
   time-mins
   ))

;(def node (->> "a + 2*1" parse-expr))
;(->> node .getRawSignature)
;(->> node loc)
;(->> node children last loc)
;
;(s/defn binary-op-spacing
;  "Find the number of spaces on either side of a binary operator"
;  [node :- IASTBinaryExpression]
;  (
;
;  )

(->> "mysql-server/rapid/unittest/gunit/xplugin/capabilities_configurator_t.cc"
     (str atom-finder-corpus-path "/")
     parse-file
     flatten-tree
     (filter binary-expr?)
     (filter confusing-operator-spacing?)
     (map (juxt type expr-operator write-tree))
     (map prn)
     )

(defn summarize-all-spaced-operators [edn-file csv-file]
  (->>
   edn-file
   read-lines
   (map (partial-right dissoc :file))
   (apply merge-with +)
   (sort-by (comp - last))
   (map (partial zipmap [:node-type :count]))
   (maps-to-csv csv-file)
   time-mins
   ))

(defn main-operator-spacing
  []
  (let [edn-file "tmp/operator-spacing_2018-11-25_02_removed_namespaces.edn"
        csv-file "src/analysis/data/operator-spacing_2018-11-25_02_removed_namespaces1.csv"
        ]
    (count-all-spaced-operators edn-file)
    ;(summarize-all-spaced-operators edn-file csv-file)
  ))
