(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTCompoundStatement IASTIfStatement IASTForStatement IASTWhileStatement))


(defn curly-braces?
  "Does this note represent curly brace?"
  [node]
  (instance? IASTCompoundStatement node))
  

(defn curly-braces-atom?
  "Does this AST node an statement that should be followed by curly braces?"
  [node]
  (let [statement #{;statement that should be followed by compound statements (curly braces)
                    IASTIfStatement IASTForStatement IASTWhileStatement}]
      (cond
        (leaf? node) false
        (some #(instance? % node) statement) (not-any? curly-braces? (children node))
        :else false)))
                           

(defn curly-braces-atoms
  "Return all AST nodes that are missing curly braces"
  [node]
  (cond
    (leaf? node) nil
    (curly-braces-atom? node) [node]
    :else (mapcat curly-braces-atoms (children node))))
