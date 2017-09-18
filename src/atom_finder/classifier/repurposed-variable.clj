(in-ns 'atom-finder.classifier)

(import '(org.eclipse.cdt.core.dom.ast IASTFunctionDeclarator IASTFunctionDefinition))

(defn repurposed-variable-finder
  ([node] (repurposed-variable-finder node #{}))
  ([node param-names]
   (let [new-param-names (clojure.set/union
                          param-names
                          (set
                           (when (instance? IASTFunctionDefinition node)
                             (map #(-> % .getDeclarator .getName node-name)
                                  (->> node .getDeclarator .getParameters)))))]

     (if (and (mutatable-op? node)
              (let [lvalue (first (children node))]
                (or ; argc = 1 OR argv[1] = 1
                 (contains? param-names (node-name lvalue))
                 (contains? param-names (node-name (first (children lvalue)))))))
       [node]
       (mapcat #(repurposed-variable-finder % new-param-names) (children node))))))

(-<>>
 "int main(int argc, char **argv) { argc[1] = 2; }"
 parse-source
 ;(get-in-tree [0 2 0 0 0])
 repurposed-variable-finder
 (map write-ast)
 )
