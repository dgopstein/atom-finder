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

(defn non-toplevel? [node] (not (instance? IASTTranslationUnit node)))

(def all-non-toplevel-preprocessor-locs (partial preprocessors-in-contexts all-preprocessor non-toplevel?))

(defn all-non-toplevel-preprocessors [root]
  (map #(offset-parent root (:offset %)) (all-non-toplevel-preprocessor-locs root)))

(defn non-toplevel-defines [root]
  (->> root
       (define-in-contexts non-toplevel?)
       (map #(->> % :offset (offset-parent root)))
       (remove nil?)))

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

(s/defn offset-range
  [node :- IASTNode]
  (when (loc node)
    (range (offset node) (inc (end-offset node)))))

(s/defn child-offsets :- #{s/Int}
  "A set of all the offsets directly owned by this node and not its children"
  [node :- IASTNode]
  (if (some->> node loc :length (< 0))
      (->> (offset-range node)
           (remove (->> node children (mapcat offset-range) set))
           set))
    #{})

(defn define-parent?
  "Is this AST node the direct parent of a preprocessor directive"
  [node]
  (->> node
       root-ancestor
       define-only
       (map offset)
       (exists? (partial offset-parent? node))
       (and (not (instance? IASTTranslationUnit node)))))

; TODO binary search the defines?? big-root has 7000 - except there might be multiple, so it has be region binary search
; or use a stateful atomfinder
(defn define-parent?-offset-sets
  "Is this AST node the direct parent of a preprocessor directive"
  [node]
  (and (not (instance? IASTTranslationUnit node))
       (->> node
            root-ancestor
            define-only
            (map offset)
            (any-pred? (child-offsets node))
            )))
