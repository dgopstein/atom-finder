(in-ns 'atom-finder.classifier)

(defn contains-location?
  "Does this node contain the given offset/length"
  [root offset length]
  (let [root-loc (.getFileLocation root)
        root-offset (.getNodeOffset root-loc)
        root-length (.getNodeLength root-loc)]

    ;; The location/offset is fully contained in this node
    (and (<=    root-offset                 offset)
         (>= (+ root-offset root-length) (+ offset length)))))

(defn contains-offset?
  "Does this node contain the given offset"
  [root offset]
  (let [root-loc (.getFileLocation root)]
    (if (nil? root-loc)
      false
      (let [root-offset (.getNodeOffset root-loc)]
        ; (According to VisualVM) The dispatch on these methods
        ; is a CPU killer. Try to short-circuit if possible.
        (if (> root-offset offset)
          false
          (>= (+ root-offset (.getNodeLength root-loc)) offset))))))

(defn offset-parent?
  "True if this is deepest AST node that contains an offset"
  [node offset]
  (and 
   (contains-offset? node offset)
   (not (exists? #(contains-offset? % offset) (children node)))))

(defn offset-parent
  "Find the AST node that contains the whole location offset
   Assumes that no children of a single parent overlap in terms of offset"
  [root offset]
  (let [kids      (children root)
        container (first (filter #(contains-offset? % offset) kids))]
    (if (nil? container)
      root
      (recur container offset))))

(defn toplevel-offset?
  "Check if an offset lives in the top level or if it's inside some other node"
  [root offset]
  (not-any? #(contains-offset? % offset) (children root)))

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

(defn all-preprocessor [node] (.getAllPreprocessorStatements (root-ancestor node)))
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

(defn statement-expression-classifier
  [parent]
  (or (instance? IASTExpression parent)
      (instance? IASTStatement parent)))

(defn expression-classifier [parent]
  (instance? IASTExpression parent))

(defn if-body-classifier [parent]
  (ancestral-instance? IASTIfStatement parent))

(defn preprocessor-in-dir
  "Find all preprocessor directives not at the top level in directory"
  [dirname]
  (time (prn (count
              (pmap-dir-nodes
               (fn [root]
                 (printf "%03d %s\n"
                         (count (preprocessors-in-contexts define-only expression-classifier root))
                         (.getFilePath root))) dirname)))))

(defn preprocessor-parent?
  "Is this AST node the direct parent of a preprocessor directive"
  [node]
  (->> node
       root-ancestor
       all-preprocessor
       (map (comp :offset loc))
       (exists? (partial offset-parent? node))))
