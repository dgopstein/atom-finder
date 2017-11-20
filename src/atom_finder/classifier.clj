(ns atom-finder.classifier
  (:require [atom-finder.util :refer :all]
            [schema.core :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [swiss.arrows :refer :all]
            )
  (:import
   [org.eclipse.cdt.core.dom.ast IASTNode IASTBinaryExpression
    IASTExpression IASTStatement IASTTranslationUnit
    IASTExpressionList IASTExpressionStatement IASTForStatement
    IASTPreprocessorMacroDefinition IASTIfStatement]
   [org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTTranslationUnit]
   [org.eclipse.cdt.internal.core.dom.rewrite.astwriter ASTWriter]
   [org.eclipse.cdt.internal.core.parser.scanner ASTMacroDefinition]
   ))

(load-cljs-in-dir "classifier/")

(def AtomName s/Keyword)
(def AtomClassifier s/Keyword)
(s/defrecord Atom [name classifier finder])

(defmacro ValidatedAtom
  "Creates an Atom record, with each function wrapped in Schema validation code"
  [name classifier finder]
  `(Atom. (s/validate AtomName ~name)
          (s/fn ~(symbol (str name "-classifier")) [node# :- IASTNode] (~classifier node#))
          (s/fn ~(symbol (str name "-finder")) :- [IASTNode] [node# :- IASTNode] (~finder node#))
          ;(s/fn ~(symbol (str name "-classifier")) :- Boolean [node# :- IASTNode]
          ;  (log-err (str "atom " ~name "-classifier") false (~classifier node#)))
          ;(s/fn ~(symbol (str name "-finder")) :- [IASTNode]  [node# :- IASTNode]
          ;  (log-err (str "atom " ~name "-finder") nil (~finder node#)))
  ))

(def atoms
  [
   (ValidatedAtom :preprocessor-in-statement stmt-or-expr-define-parent? defines-in-stmt-or-expr)
   (ValidatedAtom :logic-as-control-flow     logic-as-control-flow-atom? logic-as-control-flow-atoms)
   (ValidatedAtom :conditional               conditional-atom?           (default-finder conditional-atom?))
   (ValidatedAtom :reversed-subscript        reversed-subscript-atom?    (default-finder reversed-subscript-atom?))
   (ValidatedAtom :literal-encoding          literal-encoding-atom?      (default-finder literal-encoding-atom?))
   (ValidatedAtom :post-increment            post-*crement-atom?         (default-finder post-*crement-atom?))
   (ValidatedAtom :pre-increment             pre-*crement-atom?          (default-finder pre-*crement-atom?))
   (ValidatedAtom :comma-operator            comma-operator-atom?        (default-finder comma-operator-atom?))
   (ValidatedAtom :omitted-curly-braces      omitted-curly-braces-atom?  (default-finder omitted-curly-braces-atom?))
   (ValidatedAtom :assignment-as-value       assignment-as-value-atom?   (default-finder assignment-as-value-atom?))
   (ValidatedAtom :macro-operator-precedence macro-operator-precedence-atom?  macro-operator-precedence-finder)
   (ValidatedAtom :operator-precedence       operator-precedence-atom?   (default-finder operator-precedence-atom?))
   (ValidatedAtom :repurposed-variable       repurposed-variable-atom?   repurposed-variable-atoms)
   (ValidatedAtom :implicit-predicate        implicit-predicate-atom?    (default-finder implicit-predicate-atom?))
   (ValidatedAtom :type-conversion           type-conversion-atom?       (default-finder type-conversion-atom?))
  ])

(def atom-lookup (into {} (map #(vector (:name %1) %1) atoms)))

(def any-atom? (apply some-fn (map :classifier atoms)))

(defn find-all-atoms
  [root]
  (map-values (fn [atom]
                (log-err (str "finding " (:name atom) " in " (filename root)) nil
                         ((:finder atom) root)))
              atom-lookup))
