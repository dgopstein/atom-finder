(in-ns 'atom-finder.atom-stats)
(require '[atom-finder.patch :refer :all])
(import '(com.zutubi.diff Patch))

'(do
  (def big-commit-revstr "d4f474145ae66d041b820f4bf118601451baf261")
  (def big-commit-file "gcc/config/aarch64/arm_neon.h")
  (def big-commit-rev-commit (some-> gcc-repo (atom-finder.source-versions/find-rev-commit big-commit-revstr)))
  (def big-commit-patch-str (clj-jgit.querying/changed-files-with-patch gcc-repo big-commit-rev-commit))
  (def big-commit-srcs (when (and gcc-repo big-commit-rev-commit) (atom-finder.atom-patch/before-after-data gcc-repo big-commit-rev-commit big-commit-file)))

  (def little-commit-revstr "97574c57cf26ace9b8609575bbab66465924fef7")
  (def little-commit-file "gcc/config/sparc/sparc.c")
  (def little-commit-rev-commit (some-> gcc-repo (atom-finder.source-versions/find-rev-commit little-commit-revstr)))
  (def little-commit-patch-str (clj-jgit.querying/changed-files-with-patch gcc-repo little-commit-rev-commit))
  (def little-commit-patch (->> little-commit-patch-str parse-diff))
  (def little-commit-srcs (when (and gcc-repo little-commit-rev-commit) (atom-finder.atom-patch/before-after-data gcc-repo little-commit-rev-commit little-commit-file)))
  )

;(def a-ast (parse-resource "atoms-removed-before.c"))
;(def b-ast (parse-resource "atoms-removed-after.c"))
;(def a (->> a-ast flatten-tree-infixy))
;(def b (->> b-ast flatten-tree-infixy))
;(def post-increment (atom-finder.classifier/atom-lookup :post-increment))
;(def post-increment-finder (:finder post-increment))

(s/defn atoms-removed
  ([finder a :- IASTNode b :- IASTNode] (atoms-removed finder a b nil))
  ([finder a :- IASTNode b :- IASTNode diff-maps :- [difflib/DiffMap]]
   (let [a->b  (into {} (if diff-maps
                          (difflib/correspondence (flatten-tree-infixy a) (flatten-tree-infixy b) diff-maps)
                          (difflib/correspondence a b)))]

         (->> a finder
              (filter #(not (node= %1 (a->b %1))))
              ))))

(defn removed-atoms-stats [srcs atom]
  (let [a (:ast-before srcs)
        b (:ast-after srcs)
        diff-maps (difflib/diff-trees a b)]
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

(s/defn unchanged-ast-nodes
  [change-ranges :- [[(s/one s/Int "min") (s/one s/Int "max")]] a :- IASTNode

(s/defn patch-ast-correspondence ;:- [{IASTNode IASTNode}]
  [patch :- Patch a :- IASTNode b :- IASTNode]
  (let [changed-lines-before (->> patch deltas (map old-offset))
        changed-lines-after  (->> patch deltas (map new-offset))]

    changed-lines-after
  ))
(->> little-commit-patch
     patch-line-correspondences
     (take 1)
     pprint
     )

'(->> (patch-ast-correspondence little-commit-patch (:ast-before little-commit-srcs) (:ast-after little-commit-srcs))
     pprint
     time
     )
