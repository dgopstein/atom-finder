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
   [org.eclipse.cdt.core.dom.ast IASTNode IASTExpression IASTBinaryExpression]
   )
  )

(s/defn binary-op-spacing
  "Find the spaces on either side of a binary operator"
  [node :- IASTBinaryExpression]

  (let [sig (.getRawSignature node)
        {off :offset} (loc node)
        {end-off1 :end-offset} (->> node children first loc)
        {off2 :offset} (->> node children last loc)
        mid-sig (subs sig (- end-off1 off) (- off2 off))
        ]

    [(re-find #"^\s*" mid-sig)
     (re-find #"\s*$" mid-sig)]
    ))

(defn binary-op-n-spaces [node]
  (-<>> node binary-op-spacing
        (map #(string/replace % "\t" (string/join (repeat 8 " "))))
        (map #(string/replace % "\n" (string/join (repeat 9 " "))))
        (map count)))

(defn mixed-spacing?
  "Does a binary expression use inconsistent spacing?"
  [node]
  (let [binary-kids (->> node children (filter #(instance? IASTBinaryExpression %)))]
    (-<>> binary-kids (conj <> node) (map binary-op-spacing) (apply =) not)))

(defn confusing-operator-spacing?
  [node]
  (let [[left right] (children node)
        [space-left space-right] (binary-op-n-spaces node)]

    (and (not-any? misparsed-template? [(parent node) node left right])
         (->> node expr-operator :name (#{:shiftLeft}) not)

         (or
          (and (instance? IASTBinaryExpression left)
               (< space-left (->> left binary-op-n-spaces first)))
          (and (instance? IASTBinaryExpression right)
               (< space-right (->> right binary-op-n-spaces last)))
          ))))

(defn count-all-spaced-operators
  [edn-file]
  (println (str (now)))
  (->> atom-finder-corpus-path
   expand-home
   (pmap-dir-c-files
    (fn [file]
      (let [binaries
            (->> file parse-file flatten-tree
                 (filter (fn [node]
                           (and (instance? IASTBinaryExpression node)
                                (some (partial instance? IASTBinaryExpression) (children node)))))
                 ;(filter arithmetic-expr?)
                 (remove #(re-find #"\n|\t" (.getRawSignature %)))
                 (remove (%->> flatten-tree (some contained-by-macro-exp?)))
                 (remove nil?))]
    ;(dorun (map (fn [node] (prn [(contained-by-macro-exp? (get-in-tree [0] node)) (write-tree node)])) binaries))
        (for [binary-expr binaries]
          (log-err (str "Exception processing file: " file) nil
                   {
                    :file (atom-finder-relative-path file)
                    :line (start-line binary-expr)
                    :source (.getRawSignature binary-expr)
                    :mixed-spacing (mixed-spacing? binary-expr)
                    :confusing-spacing (confusing-operator-spacing? binary-expr)
                    }
                   )))))
   (apply concat)
   (map prn)
   dorun
   (log-to edn-file)
   time-mins
   ))

'(->> "~/opt/src/atom-finder/mysql-server/rapid/plugin/group_replication/libmysqlgcs/src/bindings/xcom/gcs_xcom_networking.cc"
     expand-home
     parse-file
     flatten-tree
     (filter (partial instance? IASTBinaryExpression))
     (filter confusing-operator-spacing?)
     ;(map write-tree)
     (map (comp misparsed-template? parent))
     (map prn)
     )

(defn summarize-all-spaced-operators [edn-file csv-file]
  (->>
   edn-file
   read-lines
   (remove nil?)
   (maps-to-csv csv-file)
   time-mins
   ))

(defn main-operator-spacing
  []
  (let [edn-file "tmp/operator-spacing_2018-11-26_03_individual-exprs.edn"
        csv-file "src/analysis/data/operator-spacing_2018-11-26_03_individual-exprs.csv"
        ]
    ;(count-all-spaced-operators edn-file)
    (summarize-all-spaced-operators edn-file csv-file)
  ))
