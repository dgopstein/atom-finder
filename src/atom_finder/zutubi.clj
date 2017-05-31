(ns atom-finder.zutubi
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
