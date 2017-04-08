(ns atom-finder.patch
  (:require
   [atom-finder.util :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as string])
  (:import
   [java.io ByteArrayInputStream StringReader]
   [com.zutubi.diff PatchFileParser unified.UnifiedPatchParser
    unified.UnifiedHunk
    unified.UnifiedHunk$LineType
    git.GitPatchParser]
   ))

(def parser (PatchFileParser. (GitPatchParser.)))

(defn parse-diff
  [patch]
  (.parse parser (StringReader. patch)))

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

(defn common? [line] (= UnifiedHunk$LineType/COMMON (.getType line)))
(defn deleted? [line] (= UnifiedHunk$LineType/DELETED (.getType line)))
(defn added? [line] (= UnifiedHunk$LineType/ADDED (.getType line)))

(->> "patch/97574c57cf26ace9b8609575bbab66465924fef7_partial.patch"
     resource-path
     slurp
     ;print)
     parse-diff
     .getPatches
     (map (memfn getHunks))
     (map #(map hunk-line-range-old %))
     pprint
     )

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
