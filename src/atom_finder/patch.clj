(ns atom-finder.patch
  (:require
   [atom-finder.util :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as string]
   [schema.core :as s]
   [atom-finder.zutubi :as zutubi]
   )
  (:import
   [java.io ByteArrayInputStream StringReader]
   [com.zutubi.diff PatchFileParser git.GitPatchParser
    unified.UnifiedHunk unified.UnifiedHunk$LineType unified.UnifiedPatchParser]
   ))

;(def patch (->>
;            "97574c57cf26ace9b8609575bbab66465924fef7_partial.patch"
;            ;"98103e4a9e8ae9e52751c9e96ec46e6095181b69.patch"
;            ;"b9db1ed4a901e9c0af7ac8cc5d4d933b5b9fd4b5.patch"
;            ;"bf8e44c9e49d658635b5a2ea4905333fa8845d1f.patch"
;            resource-path slurp))

;; java-diff-utils is unsuitable for patches with multiple files therefore
;; zutubi must be the default diff parser for deserializing patch files
(def parse-diff zutubi/parse-diff)

(defmulti deltas "Get deltas/hunks from a patch" class)
(s/defmethod deltas difflib.Patch :- [difflib.Delta]
  [p] (.getDeltas p))
(s/defmethod deltas com.zutubi.diff.Patch :- [com.zutubi.diff.unified.UnifiedHunk]
  [p] (.getHunks p))

;(defmulti original "get the old file data" class)
;(s/defmethod original difflib.Delta :- [difflib.Chunk]
;  [delta] (.getOriginal delta))
;(s/defmethod original com.zutubi.diff.unified.UnifiedHunk :- [difflib.Chunk]
;  [delta] (.getOriginal delta))

(defmulti old-offset "get the original file offset" class)
(s/defmethod old-offset difflib.Delta :- s/Int
  [delta]
  (let [orig-offset (->> delta .getOriginal .getPosition)]
    (if (< orig-offset 0)
      (throw (RuntimeException. "Patch doesn't have position information for the requested chunk"))
      (inc orig-offset))))
(s/defmethod old-offset com.zutubi.diff.unified.UnifiedHunk :- s/Int
  [hunk] (->> hunk .getOldOffset))

(defmulti new-offset "get the original file offset" class)
(s/defmethod new-offset difflib.Delta :- s/Int
  [delta]
  (let [orig-offset (->> delta .getRevised .getPosition)]
    (if (< orig-offset 0)
      (throw (RuntimeException. "Patch doesn't have position information for the requested chunk"))
      (inc orig-offset))))
(s/defmethod new-offset com.zutubi.diff.unified.UnifiedHunk :- s/Int
  [hunk] (->> hunk .getNewOffset))

(defn context-lines
  "Which lines are contained in this patch"
  [patch]
  (->> patch
       zutubi/parse-diff
      (mapcat #(deltas %))
      (map (fn [hunk] [(.getOldOffset hunk) (.getOldLength hunk) (.getNewOffset hunk) (.getNewLength hunk)] ))
  ))

(defn hunk-lines-old-lines
  "Pair each Hunk$Line with the file line number that goes with it"
  [hunk]
  (map vector (range-from (.getOldOffset hunk)) (.getLines hunk)))

(defn hunk-lines-new-lines
  "Pair each Hunk$Line with the file line number that goes with it"
  [hunk]
  (map vector (range-from (.getNewOffset hunk)) (.getLines hunk)))

(defn hunk-line-range-old
  "The first and last line of every hunk"
  [hunk]
  (let [old-offset (.getOldOffset hunk)
        new-offset (.getNewOffset hunk)
        n-lines (->> hunk .getLines count)]
    [old-offset (+ old-offset n-lines)]))

(defn deleted-lines-hunk
  "Return [line-num UnifiedHunk$Line] for every deleted line in a hunk"
  [hunk]
  (->> (hunk-lines-old-lines hunk)
       (filter (fn [[line-num line]]
                 (zutubi/deleted? line)))))

(defn removed-lines
  "Which lines are removed in this patch (using Old numbers)"
  [patch]
  (reduce merge
          (for [ptch (->> patch zutubi/parse-diff)]
            { (.getOldFile ptch)
             (->> ptch deltas
                  (map deleted-lines-hunk)
                  (mapcat #(map first %))
                  )})))

(defn patch-files-old
  "list of files removed in this patch"
  [patch]
  (->> patch
       zutubi/parse-diff
       (map (memfn getOldFile))
       set
       ))

(defn patch-files-new
  "list of files removed in this patch"
  [patch]
  (->> patch
       zutubi/parse-diff
       (map (memfn getNewFile))
       set
       ))

(defn patch-files
  "list of files changed in this patch"
  [patch]
  (->> patch
       zutubi/parse-diff
       (mapcat #(vector (.getOldFile %1) (.getNewFile %1)))
       set
       ))


;===============================================

;(def hunk (->> "patch/gcc_97574c57cf26ace9b8609575bbab66465924fef7.patch" resource-path slurp zutubi/parse-diff (drop 1) first deltas first))
;(def hunk (->> "patch/gcc_97574c57cf26ace9b8609575bbab66465924fef7.patch" resource-path slurp zutubi/parse-diff first deltas (drop 1) first))

(defn parallel-hunk-lines [hunk]
  "Label each line with the line number in their original files"
  (->>
   hunk
   .getLines
   (reduce
    (fn [hs line]
      (let [h (last hs)]
        (conj hs
              {:old-idx (+ (:old-idx h) (if (zutubi/added? line) 0 1))
               :new-idx (+ (:new-idx h) (if (zutubi/deleted? line) 0 1))
               :line line
               :type (.getType line)})))
    [{:old-idx (dec (.getOldOffset hunk))
      :new-idx (dec (.getNewOffset hunk))}])
   (drop 1) ; remove the initializer
   )
  )

(defn change-bounds [hunk]
  (str "For every added/removed chunk of lines in this hunk, "
       "find where they correspond to the original files. "
       "Start lines are inclusive, and end lines are exclusive. "
       "{:old [2, 2) :new [3, 5)} means a 2-line change is "
       "inserted after old line 1, before old line 2. "
       "{:old [2, 2] :new [3, 5)} means a 2-line change is "
       "inserted instead of old line 2.")
  (for [group
        (->> hunk parallel-hunk-lines
             (partition-by (comp zutubi/common? :line))
             (filter (comp zutubi/uncommon? :line first)))]

    (let [deleted (->> group (filter (comp zutubi/deleted? :line)))
          added   (->> group (filter (comp zutubi/added?   :line)))
          deleted-idx (map :old-idx deleted)
          added-idx   (map :new-idx added)]
      {
       :old-min      (or (min-of deleted-idx)            (inc (min-of (map :old-idx added))))
       :new-min      (or (min-of added-idx)              (inc (min-of (map :new-idx deleted))))
       :old-max      (or (some-> deleted-idx max-of inc) (inc (min-of (map :old-idx added))))
       :new-max      (or (some-> added-idx max-of   inc) (inc (min-of (map :new-idx deleted))))
       }
      )))

(defn patch-change-bounds [patch] (->> patch deltas (map change-bounds)))

(defn patch-line-correspondences [patches]
  (->>
    (for [patch patches
          hunk  (deltas patch)]
      {:file (.getOldFile patch)
       :ranges (->> hunk change-bounds)})

    (group-dissoc :file)
    (map-values #(apply merge-with concat %))
    (map (fn [[k v]] (merge {:file k} v)))
  ))

(def LineRange [(s/one s/Int "min") (s/one s/Int "max")])
(def ChangeBound {:old-min s/Int :new-min s/Int :old-max s/Int :new-max s/Int})
(s/defn change-bound-to-ranges :- {:old LineRange :new LineRange}
  [change-bound :- ChangeBound]
  {:old [(:old-min change-bound) (:old-max change-bound)]
   :new [(:new-min change-bound) (:new-max change-bound)]})

(def correspondences-to-ranges
  (partial map #(update-in % [:ranges]
    (partial map (fn [x] (change-bound-to-ranges x))))))

(s/defn correspondences-to-range-lists
  [corrs]
  (->> corrs
       correspondences-to-ranges
       (map #(update-in % [:ranges] (fn [ranges] {:old (map :old ranges)
                                                  :new (map :new ranges)})))
  ))

(s/defn intersects?
  "Do two line-ranges contain the same line"
  [[s1 e1] :- LineRange [s2 e2] :- LineRange]
  (or (and (< s1 e2) (<= e2 e1))
      (and (< s2 e1) (<= e1 e2))))

(s/defn multi-intersects?
  "Do two groups of line-ranges contain the same line"
  [r1 :- [LineRange] r2 :- [LineRange]]
  (any-pred? (fn [r] (any-pred? #(intersects? r %) r2)) r1))
