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

(def patch (->>
            "97574c57cf26ace9b8609575bbab66465924fef7_partial.patch"
            ;"98103e4a9e8ae9e52751c9e96ec46e6095181b69.patch"
            ;"b9db1ed4a901e9c0af7ac8cc5d4d933b5b9fd4b5.patch"
            ;"bf8e44c9e49d658635b5a2ea4905333fa8845d1f.patch"
            resource-path slurp))

(defn context-lines
  "Which lines are contained in this patch"
  [patch]
  (->> patch
       parse-diff
      .getPatches
      (mapcat #(.getHunks %))
      (map (fn [hunk] [(.getOldOffset hunk) (.getOldLength hunk) (.getNewOffset hunk) (.getNewLength hunk)] ))
  ))

(context-lines patch)

;; Parse a .diff/.patch file for the lines of changes

(defn hunk-lines-old-lines
  "Pair each Hunk$Line with the file line number that goes with it"
  [hunk]
  (let [old-offset (.getOldOffset hunk)]
    
    (->> hunk
         .getLines
         (map vector (range-from old-offset))
              )))
       

(defn removed-lines
  "Which lines are removed in this patch"
  [patch]
  (->>
   (for [ptch (->> patch parse-diff .getPatches pap)
         hunk (.getHunks ptch)]
      (->> (hunk-lines-old-lines hunk)
           (filter (fn [[line-num line]]
                     (= UnifiedHunk$LineType/DELETED (.getType line))))
           (map #(vector (.getOldFile ptch) (first %)))
           
           )
     )
   ;(group-by first)
   ;(map-values (partial map last))
   )
  )

(->> patch removed-lines)

(->> patch
     parse-diff
     .getPatches
     (into [])
     (#(nth % 0))
     .getHunks
     (into [])
     (#(nth % 1))
     ;first
     hunk-lines-old-lines
     pprint
     )
