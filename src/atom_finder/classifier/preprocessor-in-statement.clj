(in-ns 'atom-finder.classifier)

(defn macro-in-contexts
  "find a single macro in the given AST"
  [root macro classifier]
  (let [ret (loc macro)
               ;in-expr?  (not (instance? IASTTranslationUnit (offset-parent root offset)))
                                        ;in-expr?  (not (toplevel-offset? root offset))
        parent (offset-parent root (ret :offset))
        in-expr? (classifier parent)]

            [ret in-expr?]))

(defn preprocessors-in-contexts
  "return a list of all preprocessor directives inside of various contexts"
  [preprocessor-type context-classifier root]
    (keep (fn [[md in-expr?]] (if in-expr? md))
          (for [md (preprocessor-type root)]
            (macro-in-contexts root md context-classifier))))

(defn define-only [root] (filter #(instance? IASTPreprocessorMacroDefinition %) (all-preprocessor root)))

(defn define-in-contexts
  [context-classifier root]
  (preprocessors-in-contexts define-only context-classifier root))

(defn non-toplevel-classifier
  [parent]
  (not (instance? IASTTranslationUnit parent)))

(def all-non-toplevel-preprocessor-locs (partial preprocessors-in-contexts all-preprocessor non-toplevel-classifier))

(defn all-non-toplevel-preprocessors [root]
  (map #(offset-parent root (:offset %)) (all-non-toplevel-preprocessor-locs root)))

(defn non-toplevel-defines [root]
  (map #(->> % :offset (offset-parent root))
       (define-in-contexts non-toplevel-classifier root)))

(defn statement-expression-classifier
  [parent]
  (or (instance? IASTExpression parent)
      (instance? IASTStatement parent)))

(defn expression-classifier [parent]
  (instance? IASTExpression parent))

(defn if-body-classifier [parent]
  (ancestral-instance? IASTIfStatement parent))

(defn preprocessor-parent? ; TODO slow on big files
  "Is this AST node the direct parent of a preprocessor directive"
  [node]
  (->> node
       root-ancestor
       all-preprocessor
       (map offset)
       (exists? (partial offset-parent? node))))

(defn define-parent?
  "Is this AST node the direct parent of a preprocessor directive"
  [node]
  (->> node
       root-ancestor
       (define-in-contexts non-toplevel-classifier)
       (map :offset)
       (exists? (partial offset-parent? node))))
