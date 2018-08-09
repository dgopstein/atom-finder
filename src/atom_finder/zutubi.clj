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

(def git-parser (PatchFileParser. (GitPatchParser.)))
(def unified-parser (PatchFileParser. (UnifiedPatchParser.)))

(defn parse-git-diff [patch]
  (.getPatches (.parse git-parser (StringReader. patch))))

(defn parse-unified-diff [patch]
  (.getPatches (.parse unified-parser (StringReader. patch))))

(def parse-diff parse-git-diff)

(defn deleted? [line] (= UnifiedHunk$LineType/DELETED (.getType line)))
(defn added? [line] (= UnifiedHunk$LineType/ADDED (.getType line)))
(defn common? [line] (= UnifiedHunk$LineType/COMMON (.getType line)))
(def uncommon? (complement common?))
