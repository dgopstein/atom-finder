(ns atom-finder.patch
  (:require
   [atom-finder.util :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as string])
  (:import 
   [java.io ByteArrayInputStream StringReader]
   [com.zutubi.diff PatchFileParser unified.UnifiedPatchParser git.GitPatchParser]
   ))

(def parser (PatchFileParser. (GitPatchParser.)))

(defn parse-diff
  [patch]
  (.parse parser (StringReader. patch)))

(def patch (->> "97574c57cf26ace9b8609575bbab66465924fef7_partial.patch" resource-path slurp))
(parse-diff patch)

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

(defn removed-lines
  "Which lines are removed in this patch"
  [patch]
    
  )
