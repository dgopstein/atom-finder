(in-ns 'atom-finder.util)

(import '(org.eclipse.cdt.internal.core.dom.rewrite
          ASTModificationStore astwriter.ASTWriter astwriter.ASTWriterVisitor
          commenthandler.NodeCommentMap changegenerator.ChangeGeneratorWriterVisitor))

(defn print-tree [node]
  (letfn
      [(f [node index]
         (let [offset (format " (offset: %s, %s)"
                              (-> node .getFileLocation .getNodeOffset)
                              (-> node .getFileLocation .getNodeLength))]

           (printf "%s -%s %s -> %s\n"
                   (apply str (repeat index "  "))
                   (-> node .getClass .getSimpleName)
                   offset
                   (-> node .getRawSignature
                       (str "           ")
                       (.subSequence 0 10)
                       (.replaceAll "\n" " \\ ")))))]

    (pre-tree f node)))

(def ast-writer (ASTWriter.))
(defn write-ast [node] (.write ast-writer node))

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

(defn write-node [node]
  (let [writer-visitor (SingleNodeVisitor)]
		(when (not (nil? node))
			(.accept node writer-visitor))

		(let [node-str (str/replace (.toString writer-visitor) #"\s" "")]
      (if (empty? node-str)
        (str "<" (typename node) ">")
        node-str))
  ))
