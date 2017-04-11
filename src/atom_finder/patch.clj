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


;(defn changed-line-correspondance
;  [patch-source]
;  (for [patch (.getPatches patch-source)
;        hunk  (.getHunks patch)]
;
;    {(.getOldFile patch)
;    (->>
;     (reduce
;      (fn [h line]
;        {:old-idx (+ (:old-idx h) (if (added? line) 0 1))
;         :new-idx (+ (:new-idx h) (if (deleted? line) 0 1))
;         :old-lines (concat (:old-lines h) (if (common? line) [] [[(:old-idx h) line]]))
;         :new-lines (concat (:new-lines h) (if (common? line) [] [[(:new-idx h) line]]))
;         })
;
;      {:old-idx (.getOldOffset hunk) :old-lines []
;       :new-idx (.getNewOffset hunk) :new-lines []}
;
;      (.getLines hunk))
;
;     (#(dissoc % :old-idx :new-idx))
;     (map-values (partial map first))
;
;     )
;     }
;    )
;  )

(defn changed-lines-parallel
  [patch-source]
  (flatten1
   (for [patch (.getPatches patch-source)
         hunk  (.getHunks patch)]

     {(.getOldFile patch)
      (->>
       (reduce
        (fn [h line]
          {:old-idx (+ (:old-idx h) (if (added? line) 0 1))
           :new-idx (+ (:new-idx h) (if (deleted? line) 0 1))
           :old-lines (concat (:old-lines h) [[(:old-idx h) (if (added? line)   nil line)]])
           :new-lines (concat (:new-lines h) [[(:new-idx h) (if (deleted? line) nil line)]])
           })

        {:old-idx (.getOldOffset hunk) :old-lines []
         :new-idx (.getNewOffset hunk) :new-lines []}

        (.getLines hunk))

       (#(dissoc % :old-idx :new-idx))
                                        ;(map-values (partial map first))
       (map-values #(partition-by (fn [[num line]] (and line (common? line))) %))

       )}
     )
   )
  )

;(defn changed-lines-parallel
;  [patch-source]
;  (flatten1
;   (for [patch (.getPatches patch-source)
;         hunk  (.getHunks patch)]
;
;     {(.getOldFile patch)
;      (->>
;       (reduce
;        (fn [{old-idx :old-idx old-lines :old-lines
;              new-idx :new-idx new-lines :new-lines
;              first-deleted :first-deleted} line]
;          (let [[last-old-idx last-old-line] (last old-lines)
;                [last-new-idx last-new-line] (last new-lines)]
;            {:old-idx (+ old-idx (if (added? line) 0 1))
;             :new-idx (+ new-idx h (if (deleted? line) 0 1))
;             :first-deleted (if first-deleted first-deleted)
;             :old-lines (cond
;                          (and (common? last-old) (deleted? line))
;                          )
;             :new-lines (concat new-lines [[new-idx (if (deleted? line) nil line)]])
;             }))
;
;        {:old-idx (.getOldOffset hunk) :old-lines []
;         :new-idx (.getNewOffset hunk) :new-lines []}
;
;        (.getLines hunk))
;
;       (#(dissoc % :old-idx :new-idx))
;       ;(map-values (partial map first))
;       (map-values #(partition-by (fn [[num line]] (and line (common? line))) %))
;
;       )}
;     )
;   )
;  )


(->> "patch/97574c57cf26ace9b8609575bbab66465924fef7_partial.patch" resource-path slurp parse-diff
     changed-lines-parallel
     first
     last
     (map-values (partial map (partial map first)))
     pprint
     )

;(def hunk (->> "patch/gcc_97574c57cf26ace9b8609575bbab66465924fef7.patch"
;               resource-path slurp parse-diff .getPatches (drop 1) first .getHunks first))

;(defn parallel-hunk-lines [hunk]
;  (->
;    (reduce
;     (fn [h line]
;       {:old-idx (+ (:old-idx h) (if (added? line) 0 1))
;        :new-idx (+ (:new-idx h) (if (deleted? line) 0 1))
;        :old-lines (if (common? line)
;                     (:old-lines h)
;                     (conj (:old-lines h) [(:old-idx h) line]))
;        :new-lines (if (common? line)
;                     (:new-lines h)
;                     (conj (:new-lines h) [(:new-idx h) line]))
;        })
;     {:old-idx (.getOldOffset hunk)
;      :new-idx (.getNewOffset hunk)
;      :old-lines []
;      :new-lines []}
;     (.getLines hunk))
;
;    (dissoc :old-idx :new-idx)
;    ))

;(defn parallel-hunk-lines [hunk]
;  (->
;   (reduce
;    (fn [h line]
;      {:old-idx (+ (:old-idx h) (if (added? line) 0 1))
;       :new-idx (+ (:new-idx h) (if (deleted? line) 0 1))
;       :lines (conj (:lines h)
;                    {:old-idx (if (added? line) nil (:old-idx h))
;                     :new-idx (if (deleted? line) nil (:new-idx h))
;                     :line line})})
;    {:old-idx (dec (.getOldOffset hunk))
;     :new-idx (dec (.getNewOffset hunk))
;     :lines []}
;    (.getLines hunk))
;
;   :lines
;   ))

(defn parallel-hunk-lines [hunk]
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
    [{:old-idx (.getOldOffset hunk)
      :new-idx (.getNewOffset hunk)}])
   (drop 1)
   )
  )

(->> hunk parallel-hunk-lines
     pprint)

(defn min-of [lst]
  (if (empty? lst) nil
    (apply min lst)))

(defn max-of [lst]
  (if (empty? lst) nil
    (apply max lst)))

(defn partitioned-hunk [hunk]
  (for [group
        (->> hunk parallel-hunk-lines
             (partition-by (comp common? :line))
             (filter (comp uncommon? :line first)))]

    (let [deleted (->> group (filter (comp deleted? :line)))
          added   (->> group (filter (comp added?   :line)))
          deleted-idx (map :old-idx deleted)
          added-idx   (map :new-idx added)]
      {
       :old-min      (or (min-of deleted-idx)      (min-of (map :old-idx added)))
       :old-max (inc (or (max-of deleted-idx) (dec (min-of (map :old-idx added)))))
       :new-min      (or (min-of added-idx)        (min-of (map :new-idx deleted)))
       :new-max (inc (or (max-of added-idx)   (dec (min-of (map :new-idx deleted)))))
       }
      )))

(->> hunk partitioned-hunk)

(defn patch-lines [diff]
  (for [patch (.getPatches diff)
        hunk  (.getHunks patch)]
    (merge {:file (.getOldFile patch)}
      (->> hunk
           parallel-hunk-lines
           ((flip select-keys) [:old-lines :new-lines])
           (map-values (partial map first)) ; show only line number, remove line
           ))
    ))

(->> "patch/gcc_97574c57cf26ace9b8609575bbab66465924fef7.patch"
     resource-path slurp parse-diff
     patch-lines
     )

; test patch parser
(->> "patch/97574c57cf26ace9b8609575bbab66465924fef7_partial.patch"
     resource-path
     slurp
     parse-diff
     .getPatches
     (take 2) (drop 1)
     (map (fn [patch] (map #(vector (.getOldFile patch) (.getOldOffset %1))
                              (.getHunks patch))))
     pprint)
