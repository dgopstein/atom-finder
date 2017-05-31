(ns atom-finder.patch
  (:require
   [atom-finder.util :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as string]
   [schema.core :as s]
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

(defmulti patches "Get patches from a diff file" class)
(s/defmethod patches difflib.Patch :- [difflib.Patch]
  [p]
  ;; This model assumes each patch only contains one patch... so return it
  [p])
(s/defmethod patches com.zutubi.diff.PatchFile :- [com.zutubi.diff.Patch]
  [p] (.getPatches p))

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
  [delta] (->> delta .getOriginal .getPosition inc))
(s/defmethod old-offset com.zutubi.diff.unified.UnifiedHunk :- s/Int
  [hunk] (->> hunk .getOldOffset))

(defn context-lines
  "Which lines are contained in this patch"
  [patch]
  (->> patch
       parse-diff
       patches
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
                 (deleted? line)))))

(defn removed-lines
  "Which lines are removed in this patch (using Old numbers)"
  [patch]
  (reduce merge
          (for [ptch (->> patch parse-diff )]
            { (.getOldFile ptch)
             (->> ptch deltas
                  (map deleted-lines-hunk)
                  (mapcat #(map first %))
                  )})))

(defn patch-files-old
  "list of files removed in this patch"
  [patch]
  (->> patch
       parse-diff
       patches
       (map (memfn getOldFile))
       set
       ))

(defn patch-files-new
  "list of files removed in this patch"
  [patch]
  (->> patch
       parse-diff
       patches
       (map (memfn getNewFile))
       set
       ))

(defn patch-files
  "list of files changed in this patch"
  [patch]
  (->> patch
       parse-diff
       patches
       (mapcat #(vector (.getOldFile %1) (.getNewFile %1)))
       set
       ))


;===============================================

;(def hunk (->> "patch/gcc_97574c57cf26ace9b8609575bbab66465924fef7.patch" resource-path slurp parse-diff patches (drop 1) first deltas first))
;(def hunk (->> "patch/gcc_97574c57cf26ace9b8609575bbab66465924fef7.patch" resource-path slurp parse-diff patches first deltas (drop 1) first))

(defn parallel-hunk-lines [hunk]
  "Label each line with the line number in their original files"
  (->>
   hunk
   .getLines
   (reduce
    (fn [hs line]
      (let [h (last hs)]
        (conj hs
              {:old-idx (+ (:old-idx h) (if (added? line) 0 1))
               :new-idx (+ (:new-idx h) (if (deleted? line) 0 1))
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
             (partition-by (comp common? :line))
             (filter (comp uncommon? :line first)))]

    (let [deleted (->> group (filter (comp deleted? :line)))
          added   (->> group (filter (comp added?   :line)))
          deleted-idx (map :old-idx deleted)
          added-idx   (map :new-idx added)]
      {
       :old-min      (or (min-of deleted-idx)            (inc (min-of (map :old-idx added))))
       :new-min      (or (min-of added-idx)              (inc (min-of (map :new-idx deleted))))
       :old-max      (or (some-> deleted-idx max-of inc) (inc (min-of (map :old-idx added))))
       :new-max      (or (some-> added-idx max-of   inc) (inc (min-of (map :new-idx deleted))))
       }
      )))

;(->> hunk change-bounds)

(defn patch-correspondences [diff]
  (->>
    (for [patch (patches diff)
          hunk  (deltas patch)]
      {:file (.getOldFile patch)
       :ranges (->> hunk change-bounds)})

    (group-dissoc :file)
    (map-values #(apply merge-with concat %))
    (map (fn [[k v]] (merge {:file k} v)))
  ))
;(map-kv #(vector (:file %1) (:ranges %1)))

(def correspondences-to-ranges
  (partial map #(update-in % [:ranges]
    (partial map (fn [x] {:old [(:old-min x) (:old-max x)]
                          :new [(:new-min x) (:new-max x)]})))))
