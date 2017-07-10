(ns atom-finder.tree-diff.change-distiller
  (:require [atom-finder.util :refer :all]
            [clojure.string :as str]
            [schema.core :as s]
            )
  (:use     [clojure.pprint :only [pprint print-table]])
  (:import [org.eclipse.cdt.core.dom.ast IASTNode IASTExpression
            IASTCompoundStatement IASTComment IASTBreakStatement IASTCastExpression
            IASTConditionalExpression IASTContinueStatement
            IASTDoStatement IASTFieldReference IASTFieldDeclarator IASTForStatement IASTIfStatement
            IASTBinaryExpression IASTLabelStatement IASTFunctionDefinition IASTFunctionDeclarator
            IASTFunctionCallExpression IASTLiteralExpression IASTParameterDeclaration IASTUnaryExpression
            IASTReturnStatement IASTName IASTTypeId IASTCaseStatement IASTSwitchStatement
            IASTDeclarationStatement IASTWhileStatement]
           [org.eclipse.cdt.core.dom.ast.cpp ICPPASTCatchHandler ICPPASTNewExpression ICPPASTTryBlockStatement]
           [org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTTranslationUnit]
           [ch.uzh.ifi.seal.changedistiller.treedifferencing Node TreeDifferencer]
           [ch.uzh.ifi.seal.changedistiller.model.classifiers EntityType java.JavaEntityType]
           ))


(def java-entity-type
  {
   ;ARGUMENTS
   ;ARRAY_ACCESS
   ;ARRAY_CREATION
   ;ARRAY_INITIALIZER
   ;ARRAY_TYPE
   ;ASSERT_STATEMENT
   ;ASSIGNMENT
   ;FIELD
   IASTCompoundStatement JavaEntityType/BLOCK
   IASTComment JavaEntityType/BLOCK_COMMENT
   ;BODY
   ;BOOLEAN_LITERAL
   IASTBreakStatement JavaEntityType/BREAK_STATEMENT
   IASTCastExpression JavaEntityType/CAST_EXPRESSION
   ;ICPPASTCatchHandler CATCH_CLAUSE
   ;CATCH_CLAUSES
   ;CHARACTER_LITERAL
   ;CLASS
   ;CLASS_INSTANCE_CREATION
   ;COMPILATION_UNIT
   IASTConditionalExpression JavaEntityType/CONDITIONAL_EXPRESSION
   ICPPASTNewExpression JavaEntityType/CONSTRUCTOR_INVOCATION
   IASTContinueStatement JavaEntityType/CONTINUE_STATEMENT
   IASTDoStatement JavaEntityType/DO_STATEMENT
   ;ELSE_STATEMENT
   ;EMPTY_STATEMENT
   ;FOREACH_STATEMENT
   IASTFieldReference JavaEntityType/FIELD_ACCESS
   IASTFieldDeclarator JavaEntityType/FIELD_DECLARATION
   ;FINALLY
   IASTForStatement JavaEntityType/FOR_STATEMENT
   IASTIfStatement JavaEntityType/IF_STATEMENT
   IASTBinaryExpression JavaEntityType/INFIX_EXPRESSION
   ;INSTANCEOF_EXPRESSION
   ;JAVADOC
   IASTLabelStatement JavaEntityType/LABELED_STATEMENT
   ;LINE_COMMENT
   IASTFunctionDefinition JavaEntityType/METHOD
   IASTFunctionDeclarator JavaEntityType/METHOD_DECLARATION
   IASTFunctionCallExpression JavaEntityType/METHOD_INVOCATION
   ;MODIFIER
   ;MODIFIERS
   ;NULL_LITERAL
   IASTLiteralExpression JavaEntityType/NUMBER_LITERAL
   ;PARAMETERIZED_TYPE
   ;PARAMETERS
   IASTParameterDeclaration JavaEntityType/PARAMETER
   ;POSTFIX_EXPRESSION
   IASTUnaryExpression JavaEntityType/PREFIX_EXPRESSION
   ;PRIMITIVE_TYPE
   ;QUALIFIED_NAME
   ;QUALIFIED_TYPE
   IASTReturnStatement JavaEntityType/RETURN_STATEMENT
   ;ROOT_NODE
   IASTName JavaEntityType/SIMPLE_NAME
   IASTTypeId JavaEntityType/SINGLE_TYPE
   ;STRING_LITERAL
   ;SUPER_INTERFACE_TYPES
   IASTCaseStatement JavaEntityType/SWITCH_CASE
   IASTSwitchStatement JavaEntityType/SWITCH_STATEMENT
   ;SYNCHRONIZED_STATEMENT
   ;THEN_STATEMENT
   ;THROW
   ;THROW_STATEMENT
   ICPPASTTryBlockStatement JavaEntityType/TRY_STATEMENT
   ;TYPE_PARAMETERS
   ;TYPE_DECLARATION
   ;TYPE_LITERAL
   ;TYPE_PARAMETER
   IASTDeclarationStatement JavaEntityType/VARIABLE_DECLARATION_STATEMENT
   IASTWhileStatement JavaEntityType/WHILE_STATEMENT
   ;WILDCARD_TYPE
   ;FOR_INIT
   ;FOR_INCR
   })

(defn distiller-type [node]
  (or
   (some (fn [[k v]] (and (instance? k node) v)) java-entity-type)
   JavaEntityType/WILDCARD_TYPE))

(defn new-ccd [node label]
  (let [ccd (atom_finder.CDTChangeDistillerNode. (distiller-type node) node)]
    (doseq [child (children node)]
      (.add ccd (new-ccd child (typename child))))
    ccd))

(defn new-root [node]
  (let [ccd (new-ccd node "<new-root>")]
    (.setLabel ccd JavaEntityType/ROOT_NODE)
  ccd))

(s/defn tree-diff
  "Construct a TreeDifferencer and calculate its edit script"
  [left :- IASTNode right :- IASTNode]
  (let [tree-differ (TreeDifferencer.)]
    (.calculateEditScript tree-differ (new-root left) (new-root right))
    tree-differ
  ))

(s/defn tree-correspondences
  "For two trees, associate each node with its closest pair in the other tree"
  ([tree-differ :- TreeDifferencer]
   (->> (private-field tree-differ "fMatch")
        (map (fn [pair] [(.getLeft pair) (.getRight pair)]))
        (map (partial map (memfn node)))
        ))
  ([left :- IASTNode right :- IASTNode]
   (tree-correspondences (tree-diff left right))))

(defn left->right
  [correspondences]
  (->> correspondences
       (map (partial into []))
       (into {})))

(def right->left (comp clojure.set/map-invert left->right))
