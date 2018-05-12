;; Convert a C/C++/Header file into an edn representation containing the
;; structure and (partial) type information of the AST

(ns quark.tree-tokenizer
  (:require [atom-finder.util :refer :all]
            [atom-finder.constants :as constants]
            [schema.core :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [swiss.arrows :refer :all]
            [clj-cdt.clj-cdt :refer :all]
            [clj-cdt.writer-util :refer :all]
            )
  (:import
   [org.eclipse.cdt.core.dom.ast IASTNode IASTBinaryExpression
    IASTExpression IASTStatement IASTTranslationUnit IASTName
    IASTExpressionList IASTExpressionStatement IASTForStatement
    IASTPreprocessorMacroDefinition IASTIfStatement IASTUnaryExpression
    IProblemBinding IProblemType]
   [org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTTranslationUnit]
   [org.eclipse.cdt.internal.core.dom.rewrite.astwriter ASTWriter]
   [org.eclipse.cdt.internal.core.parser.scanner ASTMacroDefinition]
   ))

(defn expr-typename
  [node]
  (log-err (str "Exception getting expression type for [file node] "
                [(filename node) (tree-path node)]) "---exception---"
        (if (instance? IASTExpression node)
           (let [expr-type (-> node .getExpressionType)]
             (condp instance? expr-type
               IProblemBinding "problem-binding"
               IProblemType "problem-type"
               (str expr-type)))
           nil)))

(defmulti node-to-edn "Convert IASTNode node to plain-old-clojure-object's" class)

(s/defmethod node-to-edn clojure.lang.ISeq [node] (-> node first node-to-edn))
(s/defmethod node-to-edn nil [node] [nil nil nil])
(s/defmethod node-to-edn :default [node] [(typename node) (expr-typename node) (write-node node)])

(s/defmethod node-to-edn IASTExpression [node] [(typename node) (expr-typename node) (write-node node)])
(s/defmethod node-to-edn IASTName [node] [(typename node) (->> node node-name)])

(defn tree-to-edn
  "Serialize AST node into an edn list"
  [node]
  (let [edn (node-to-edn node)]
    (if (leaf? node)
      edn
      (cons edn (map tree-to-edn (children node))))))

(defn src-dir-trees-to-edn
  [unexpanded-src-path unexpanded-out-path]
  (doseq [:let [src-path (expand-home unexpanded-src-path)
                out-path (expand-home unexpanded-out-path)]
          src-file (c-files src-path)
          :let [src-filename (.getAbsolutePath src-file)
                out-filename (str (str/replace src-filename src-path out-path) ".edn")]]
    (clojure.java.io/make-parents out-filename)
    (->> src-filename parse-file tree-to-edn (spit out-filename))))

'((time-mins (src-dir-trees-to-edn linux-path "tmp/src-tree-to-edn/linux4")))

;(defn all-child-paths
;  "given a directory path, find all relative paths under it"
;  []
;  (let [{true dirs false files}
;        (->> "~/nyu/confusion/atom-finder/" expand-home files-in-dir
;             (group-by (memfn isDirectory)))])

(defn split-path [path] (str/split path #"/"))

(defn all-child-paths
  "given a directory path, find all relative paths under it"
  [path]
  (-<>> path files-in-dir (map (memfn getCanonicalPath))
        ;(map #(str/replace % (re-pattern (str path "/?")) "")) ;; strip leading paths
        ))

(defn included-files
  "extract the files included from this file"
  [filename]
  (->> filename
       slurp-lines
       (map (partial re-find #"#include *[\"<]([^\">]*)[\">]"))
       (remove nil?)
       (map last)))

(def include-files (->> "~/opt/src/mongo/src/mongo/base/parse_number.cpp" expand-home included-files))

(defn infer-include-paths
  [all-paths include-paths]
  (->> include-paths
       (filter #(.contains % "/"))
       (mapcat (fn [include-path]
                 (->> all-paths
                      (map (fn [child-path] [child-path (str/index-of child-path include-path)]))
                      (remove (comp nil? last))
                      (map (fn [[path idx]] (subs path 0 idx)))
                      )))
       distinct))

(defn parent-dir [path] (->> path java.io.File. .getParent))

(defn parse-file-with-proj-includes
  "infer which project-level include paths might be useful then parse with them"
  [all-paths file]
  (let [include-paths (infer-include-paths all-paths include-files)]
    (parse-file file {:include-dirs (concat [(parent-dir file)]
                                            constants/system-include-paths
                                            include-paths
                                            )})))

(def mongo-files (->> "~/opt/src/mongo" expand-home all-child-paths))


;; percentage of expressions with type information
'((->> "/Users/dgopstein/opt/src/mongo/src/mongo/base/secure_allocator.cpp"
     expand-home
     ;(parse-file-with-proj-includes mongo-files)
     parse-file
     flatten-tree
     (remove from-include?)
     (filter (partial instance? IASTExpression))
     (map expr-typename)
     frequencies
     (group-by (comp not #(str/starts-with? % "problem-") first))
     (map-values (%->> (map last) sum))
     pap
     ((fn [{t true f false}] (prn (float (/ t (+ t f))) " of " (+ t f))))
     time-mins))

;; which expressions don't have type information
'((->> "/Users/dgopstein/opt/src/mongo/src/mongo/base/secure_allocator.cpp"
     expand-home
     (parse-file-with-proj-includes mongo-files)
     flatten-tree
     (remove from-include?)
     (filter (partial instance? IASTExpression))
     (map tree-to-edn)
     (map prn)
     dorun
     time-mins))

(defn src-dir-trees-to-edn-with-includes-filtered
  [unexpanded-src-path unexpanded-out-path]
  (dorun (doseq [:let [src-path (expand-home unexpanded-src-path)
                out-path (expand-home unexpanded-out-path)
                src-files (c-files src-path)]
          src-file (c-files src-path)
          :let [src-filename (.getAbsolutePath src-file)
                out-filename (str (str/replace src-filename src-path out-path) ".edn")]]

    (clojure.java.io/make-parents out-filename)

    (->> src-filename
         pprn
         (parse-file-with-proj-includes src-files)
         (remove-seq-tree from-include?)
         tree-to-edn
         (spit out-filename)
         (with-timeout 200)
         ))))

'((time-mins (src-dir-trees-to-edn-with-includes-filtered "~/nyu/confusion/atom-finder/src/test/resources" "~/nyu/confusion/atom-finder/to-edn-atom-finder")))

(defn src-csv-tree-to-edn
  [csv-path column-name]
  (with-open [reader (io/reader csv-path)
              writer (io/writer (str csv-path ".edn"))]
    (let [[header & csv-data] (csv/read-csv reader)
          code-idx (.indexOf header column-name)
          edn-data (map #(update-in % [code-idx] (comp tree-to-edn parse-source)) csv-data)]
     (csv/write-csv writer (cons header edn-data))
      )))


'((time-mins (src-csv-tree-to-edn "tmp/context_study_code.csv" "Code")))

