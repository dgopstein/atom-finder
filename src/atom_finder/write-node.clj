(ns atom-finder.core
  (:require [atom-finder.util :refer :all]
            [schema.core :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            )
  (:import )
  )

(import '(org.eclipse.cdt.internal.core.dom.rewrite.astwriter ASTWriter ASTWriterVisitor))
(import '(org.eclipse.cdt.internal.core.dom.rewrite ASTModificationStore))
(import '(org.eclipse.cdt.internal.core.dom.rewrite.commenthandler NodeCommentMap))
(import '(org.eclipse.cdt.internal.core.dom.rewrite.changegenerator ChangeGeneratorWriterVisitor))

(defmacro visit-nothing [writer]
  "Tell the AST-visitor to visit no types of node"
  (let [fields
        ['shouldVisitArrayModifiers 'shouldVisitBaseSpecifiers
         'shouldVisitDeclarations 'shouldVisitDeclarators 'shouldVisitDeclSpecifiers
         'shouldVisitDecltypeSpecifiers 'shouldVisitExpressions
         'shouldVisitInitializers 'shouldVisitNames 'shouldVisitNamespaces
         'shouldVisitParameterDeclarations 'shouldVisitPointerOperators
         'shouldVisitStatements 'shouldVisitTemplateParameters
         'shouldVisitTranslationUnit 'shouldVisitTypeIds 'shouldVisitAttributes]]
    (concat '(do)
            (for [field fields]
              `(set! (. ~writer ~field) false)))))

(defn SingleNodeVisitor []
  (let [modificationStore (ASTModificationStore.)
        commentMap (NodeCommentMap.)
        ;writer-visitor (ChangeGeneratorWriterVisitor. modificationStore nil commentMap)]
        ]
    (proxy [ChangeGeneratorWriterVisitor] [modificationStore nil commentMap]
      (visit [node]
        (visit-nothing this)
        (proxy-super visit node))
      )))

(SingleNodeVisitor)

(defn write-node [node]
  (let [writer-visitor (SingleNodeVisitor)]
		(when (not (nil? node))
			(.accept node writer-visitor))

		(.toString writer-visitor)
  ))

(->> "a + b"
     parse-frag
     write-node)
     ;write-ast)
