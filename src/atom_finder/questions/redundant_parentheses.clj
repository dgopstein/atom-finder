(ns atom-finder.questions.redundant-parentheses
  (:require
   [clj-jgit.internal  :as giti]
   [clj-jgit.porcelain :as gitp]
   [clj-jgit.querying  :as gitq]
   [atom-finder.patch :refer :all]
   [atom-finder.atom-patch :refer :all]
   [atom-finder.constants :refer :all]
   [atom-finder.util :refer :all]
   [atom-finder.classifier :refer [operator-group underclassify-confusing-operator-combination? overclassify-confusing-operator-combination?]]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as string]
   )
  (:import
   [org.eclipse.cdt.core.dom.ast IASTBinaryExpression]
   [org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTConditionalExpression]
   ))

(defn paren-in-children
  [node]
  (find-first paren-node? (children node)))

(defn redundant-paren?
  [node]
  (when-let* [paren-test (paren-node? node)
              parent-level (precedence-level (safe-parent node))
             children-level (precedence-level(first (children node)))] 
    (or (< children-level parent-level)
        (and (= children-level parent-level)
             (= node (first (children (safe-parent node))))))))

(defn atom-without-paren?
  "Given a parenthesis node, would this be an operator-precedence-atom without the parentheses?"
  [node]
  (when-let* [parent-node (safe-parent node)
              parent-group (operator-group parent-node)
              child-group (operator-group (first (children node)))
              opt-group-combination [parent-group child-group]]

             (cond 

                  (and (= :bitwise_bin parent-group)
                       (= parent-group child-group))
                  (not (= (.getOperator parent-node) (.getOperator (first (children node)))))


                 (and (or (instance? IASTBinaryExpression parent-node)
                          (instance? CPPASTConditionalExpression parent-node))
                      (= node (first (children parent-node))))
                 (underclassify-confusing-operator-combination? (reverse opt-group-combination))
               
                 :else(underclassify-confusing-operator-combination? opt-group-combination))))

(->> "(fclose(f1a) == EOF) || (fclose(f1b) | EOF)" parse-expr 
     ;print-tree
     (get-in-tree [0])
     ;print-tree
     ;redundant-paren?
     atom-without-paren?
)

(->> gcc-path
     (pmap-dir-trees (fn [root] (filter-tree ;redundant-paren?
                                 #(and (redundant-paren? %)(atom-without-paren? %))
                                 root)))
     flatten
     (remove nil?)
     (map safe-parent)
     (map write-ast)
     (map #(spit "underclas-atom-without.txt" (str % "\r\n") :append true))
     (take 100)

     ;((fn [a] (prn (count a)) a))
     ;(map atom-without-paren?)
     ;(remove (fn [input] (or (nil? input) (false? input))))

     count
     prn
     dorun
     time-mins)

;redundant-total: 392166

;Underclassify:
;would-be-atom: 66037

;Overclassify:
;would-be-atom: 75855 

