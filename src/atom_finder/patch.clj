(ns atom-finder.patch
  (:require
   [atom-finder.util :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as string])
  (:import
   [java.io ByteArrayInputStream StringReader]
   [com.zutubi.diff PatchFileParser git.GitPatchParser
    unified.UnifiedHunk unified.UnifiedHunk$LineType unified.UnifiedPatchParser]
   ))

(def parser (PatchFileParser. (GitPatchParser.)))
;(def parser (PatchFileParser. (UnifiedPatchParser.)))

(defn parse-diff
  [patch]
  (.parse parser (StringReader. patch)))

(defn deleted? [line] (= UnifiedHunk$LineType/DELETED (.getType line)))
(defn added? [line] (= UnifiedHunk$LineType/ADDED (.getType line)))
(defn common? [line] (= UnifiedHunk$LineType/COMMON (.getType line)))
(def uncommon? (complement common?))

;(def patch (->>
;            "97574c57cf26ace9b8609575bbab66465924fef7_partial.patch"
;            ;"98103e4a9e8ae9e52751c9e96ec46e6095181b69.patch"
;            ;"b9db1ed4a901e9c0af7ac8cc5d4d933b5b9fd4b5.patch"
;            ;"bf8e44c9e49d658635b5a2ea4905333fa8845d1f.patch"
;            resource-path slurp))

(defn context-lines
  "Which lines are contained in this patch"
  [patch]
  (->> patch
       parse-diff
      .getPatches
      (mapcat #(.getHunks %))
      (map (fn [hunk] [(.getOldOffset hunk) (.getOldLength hunk) (.getNewOffset hunk) (.getNewLength hunk)] ))
  ))

(defn hunk-lines-old-lines
  "Pair each Hunk$Line with the file line number that goes with it"
  [hunk]
  (let [old-offset (.getOldOffset hunk)]

    (->> hunk
         .getLines
         (map vector (range-from old-offset))
              )))

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
  "Which lines are removed in this patch"
  [patch]
  (reduce merge
          (for [ptch (->> patch parse-diff .getPatches)]
            { (.getOldFile ptch)
             (->> ptch .getHunks
                  (map deleted-lines-hunk)
                  (mapcat #(map first %))
                  )})))

(defn patch-files-old
  "list of files removed in this patch"
  [patch]
  (->> patch
       parse-diff
       .getPatches
       (map (memfn getOldFile))
       set
       ))

(defn patch-files-new
  "list of files removed in this patch"
  [patch]
  (->> patch
       parse-diff
       .getPatches
       (map (memfn getNewFile))
       set
       ))

(defn patch-files
  "list of files changed in this patch"
  [patch]
  (->> patch
       parse-diff
       .getPatches
       (mapcat #(vector (.getOldFile %1) (.getNewFile %1)))
       set
       ))


;===============================================

;(def hunk (->> "patch/gcc_97574c57cf26ace9b8609575bbab66465924fef7.patch" resource-path slurp parse-diff .getPatches (drop 1) first .getHunks first))
;(def hunk (->> "patch/gcc_97574c57cf26ace9b8609575bbab66465924fef7.patch" resource-path slurp parse-diff .getPatches first .getHunks (drop 1) first))

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

(->> hunk change-bounds)

(defn patch-correspondences [diff]
  (->>
    (for [patch (.getPatches diff)
          hunk  (.getHunks patch)]
      {:file (.getOldFile patch)
       :ranges (->> hunk change-bounds
                    (map (fn [x] {:old [(:old-min x) (:old-max x)]
                                  :new [(:new-min x) (:new-max x)]})))})
    (group-dissoc :file)
    (map-values #(apply merge-with concat %))
    (map (fn [[k v]] (merge {:file k} v)))
  ))

;(->> hunk parallel-hunk-lines (map #(update-in (dissoc % :line) [:type] str)) pprint)

;(->> "patch/gcc_97574c57cf26ace9b8609575bbab66465924fef7.patch"
;     resource-path slurp parse-diff
;     patch-correspondences
;     pprint)

