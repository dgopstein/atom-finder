(in-ns 'atom-finder.atom-stats)

'(do
  (def big-commit-revstr "d4f474145ae66d041b820f4bf118601451baf261")
  (def big-commit-file "gcc/config/aarch64/arm_neon.h")
  (def big-commit-rev-commit (some-> gcc-repo (atom-finder.source-versions/find-rev-commit big-commit-revstr)))
  (def big-commit-patch-str (clj-jgit.querying/changed-files-with-patch gcc-repo big-commit-rev-commit))
  (def big-commit-srcs (when (and gcc-repo big-commit-rev-commit) (atom-finder.atom-patch/before-after-data gcc-repo big-commit-rev-commit big-commit-file)))
  )

;(def a-ast (parse-resource "atoms-removed-before.c"))
;(def b-ast (parse-resource "atoms-removed-after.c"))
;(def a (->> a-ast flatten-tree-infixy))
;(def b (->> b-ast flatten-tree-infixy))
;(def post-increment (atom-finder.classifier/atom-lookup :post-increment))
;(def post-increment-finder (:finder post-increment))

(s/defn atoms-removed
  ([finder a :- IASTNode b :- IASTNode] (atoms-removed finder a b nil))
  ([finder a :- IASTNode b :- IASTNode diff-maps :- [DiffMap]]
   (let [a->b  (into {} (if diff-maps
                          (correspondence (flatten-tree-infixy a) (flatten-tree-infixy b) diff-maps)
                          (correspondence a b)))]

         (->> a finder
              (filter #(not (node= %1 (a->b %1))))
              ))))

(defn removed-atoms-stats [srcs atom]
  (let [a (:ast-before srcs)
        b (:ast-after srcs)
        diff-maps (diff-trees a b)]
  {:removed-nodes (->> diff-maps (filter #(= :delete (:type %))) count)
   :removed-atoms (count (atoms-removed (:finder atom) a b diff-maps))
   }))

;(->> big-commit-srcs :ast-before flatten-tree count) => 151033 => O(22billion)

'(->> ;(removed-atoms-stats big-commit-srcs post-increment)
     (diff-trees (:ast-before big-commit-srcs) (:ast-after big-commit-srcs))
     pprint
     time
     )

;(->> big-commit-patch-str atom-finder.tree-diff.difflib/parse-patch .getDeltas first)

'(s/defn patch-correspondence :- [{IASTNode IASTNode}]
  [s :- s/Str a :- IASTNode b :- IASTNode]
  (let [lines-before 
        lines-after]
  ))


;(removed-atoms-stats
; {:ast-before (parse-frag "1, f(a + b + c)")
;  :ast-after (parse-frag "1 + 2")}
; post-increment)
;(time (pprint (removed-atoms-stats srcs post-increment)))
;(def srcs (atom-finder.atom-patch/before-after-data gcc-repo (atom-finder.source-versions/find-rev-commit gcc-repo "519d8cb612e9d641d133e4f65b4a48c3ef963f43") "gcc/lto-cgraph.c"))

;(->> (correspondence a b) (def corrs))
;(->> a-ast post-increment-finder ((flip nth) 2) write-ast)
;(->> corrs (map (fn [[k v]] [(write-ast k) (write-ast v)])) pprint)
;(->> (atoms-removed post-increment-finder a-ast b-ast)
;     (map write-ast))
