(in-ns 'atom-finder.atom-stats)

(s/defn atom-comments
  "For every atom, collect the comments that are near them"
  ([atom-nodes :- [IASTNode] comments :- [IASTComment]]
    (->>
     (loop [atoms atom-nodes
            comments comments
            atm-cmnts []]
       (if (or (empty? atoms) (empty? comments))
         atm-cmnts
         (let [look-behind 3 ; how far behind an atom an associated comment can be
               atom-line (end-line (first atoms))
               cmnt-line (end-line (first comments))
               past-comments #(<= (end-line %)  atom-line)]
           (recur (rest atoms)
                  (drop-while past-comments comments)
                  (conj atm-cmnts
                        [(first atoms)
                         (->> comments
                              (take-while past-comments)
                              (drop-while #(< (+ look-behind (end-line %)) atom-line)))]
                        )))))
     (filter (comp not empty? last))
     )))

(defn atom-finder-comments
  [atom root]
  (atom-comments ((:finder atom) root) (all-comments root)))

(s/defn comments-added
  "Which comments were added near atoms"
  [srcs]
  (->>
   (diff-by str (->> srcs :ast-before all-comments) (->> srcs :ast-after all-comments))
   (filter #(->> % :delta .getType (= Delta$TYPE/INSERT)))
   (mapcat :revised)
   ))

(defn atom-comments-added
  [srcs]
   (atom-comments (->> srcs :atoms-after) (comments-added srcs)))

;(defn added-comments [srcs atom]
;  (let [cmnts-added (comments-added srcs)
;        atom-cmnts-added (atom-comments (:atoms-after srcs) cmnts-added)
;        n-cmnts-added (count cmnts-added)
;        n-atom-cmnts-added (count atom-cmnts-added)]
;  {:comments-added n-cmnts-added
;   :comments-added-near-atoms n-atom-cmnts-added
;   :comments-added-away-atoms (- n-cmnts-added n-atom-cmnts-added)}))

;(def srcs (atom-finder.atom-patch/before-after-data gcc-repo (atom-finder.source-versions/find-rev-commit gcc-repo "519d8cb612e9d641d133e4f65b4a48c3ef963f43") "gcc/lra-constraints.c"))
;(added-comments-context srcs (atom-finder.classifier/atom-lookup :omitted-curly-braces))

(defn added-comments-context [srcs atom]
  (let [cmnts-added (comments-added srcs)
        {inner-cmnts-added true outer-cmnts-added false} (group-by in-function? (map offset-parent cmnts-added))
        {inner-atoms true outer-atoms false} (group-by in-function? (:atoms-after srcs))
        {inner-ast true outer-ast false} (group-by in-function? (flatten-tree (:ast-after srcs)))
        inner-atom-cmnts-added (atom-comments inner-atoms cmnts-added)
        outer-atom-cmnts-added (atom-comments outer-atoms cmnts-added)
        n-cmnts-added (count cmnts-added)
        n-inner-cmnts-added (count inner-cmnts-added)
        n-outer-cmnts-added (count outer-cmnts-added)
        n-inner-atom-cmnts-added (count inner-atom-cmnts-added)
        n-outer-atom-cmnts-added (count outer-atom-cmnts-added)]
  {:comments-added n-cmnts-added
   :inner-comments-added n-inner-cmnts-added
   :outer-comments-added n-outer-cmnts-added

   :comments-added-near-atoms (+ n-inner-atom-cmnts-added n-outer-atom-cmnts-added)
   :inner-comments-added-near-atoms n-inner-atom-cmnts-added
   :outer-comments-added-near-atoms n-outer-atom-cmnts-added

   ;:comments-added-away-atoms (- n-cmnts-added n-inner-atom-cmnts-added n-outer-atom-cmnts-added)
   ;:inner-comments-added-away-atoms (- n-inner-cmnts-added n-inner-atom-cmnts-added)
   ;:outer-comments-added-away-atoms (- n-outer-cmnts-added n-outer-atom-cmnts-added)

   :inner-atom-count (count inner-atoms)
   :outer-atom-count (count outer-atoms)
   :inner-ast-size (count inner-ast)
   :outer-ast-size (count outer-ast)
   }))
