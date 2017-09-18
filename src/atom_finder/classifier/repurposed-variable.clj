(in-ns 'atom-finder.classifier)

(import '(org.eclipse.cdt.core.dom.ast IASTFunctionDeclarator IASTFunctionDefinition))

(defn fn-param-names
  "names of the parameters of the given function"
  [node]
  (when (function-node? node)
    (set (map #(-> % .getDeclarator .getName node-name)
              (->> node .getDeclarator .getParameters)))))

(defn repurposed-variable-atom?
  ([node] (repurposed-variable-atom? node (fn-param-names (enclosing-function node))))
  ([node param-names]
   (and (mutatable-op? node)
        (let [lvalue (first (children node))]
          (or ; argc = 1 OR argv[1] = 1
           (contains? param-names (node-name lvalue))
           (contains? param-names (node-name (first (children lvalue)))))))))

(defn repurposed-variable-atoms
  ([root] (repurposed-variable-atoms root #{}))
  ([root param-names]
   (let [new-param-names (clojure.set/union param-names (fn-param-names root))]
     (if (repurposed-variable-atom? root param-names)
       [root]
       (mapcat #(repurposed-variable-atoms % new-param-names) (children root))))))
