(ns atom-finder.patch-test
  (:require [clojure.test :refer :all]
            [atom-finder.util :refer :all]
            [atom-finder.constants :refer :all]
            [atom-finder.patch :refer :all]
            [atom-finder.results-util :refer :all]
            [clojure.pprint :refer :all]
            ))

(deftest hunk-line-ranges-test
  (testing "Which lines were added and removed in a hunk"
    (is (=
         '({:file "gcc/ChangeLog"
            :ranges
            ({:old [0 0] :new [1 12]}
             {:old [15 16] :new [27 28]}
             {:old [26 29] :new [38 40]})}
           {:file "/gcc/config/sparc/sparc.c"
            :ranges
            ({:old [650 650] :new [650 651]}
             {:old [869 869] :new [870 872]}
             {:old [2753 2753] :new [2756 2763]})})

         (->> "patch/97574c57cf26ace9b8609575bbab66465924fef7_partial.patch"
              parse-diff hunk-line-ranges)
         ))
    ))

(defn changed-line-correspondance
  [patch-source]
  (for [patch (.getPatches patch-source)
        hunk  (.getHunks patch)]

    {(.getOldFile patch)
    (->>
     (reduce
      (fn [h line]
        {:old-idx (+ (:old-idx h) (if (added? line) 0 1))
         :new-idx (+ (:new-idx h) (if (deleted? line) 0 1))
         :old-lines (concat (:old-lines h) (if (common? line) [] [[(:old-idx h) line]]))
         :new-lines (concat (:new-lines h) (if (common? line) [] [[(:new-idx h) line]]))
         })

      {:old-idx (.getOldOffset hunk) :old-lines []
       :new-idx (.getNewOffset hunk) :new-lines []}

      (.getLines hunk))

     (#(dissoc % :old-idx :new-idx))
     (map-values (partial map first))

     )
     }
    )
  )

(->> "patch/97574c57cf26ace9b8609575bbab66465924fef7_partial.patch"
     resource-path
     slurp
     parse-diff
     changed-line-correspondance
     )

(->> "patch/97574c57cf26ace9b8609575bbab66465924fef7_partial.patch"
     resource-path
     slurp
     ;println)
     parse-diff
     .getPatches
     (map (memfn getHunks))
     (map #(map
            (fn [hunk]
              (reduce
                  (fn [h line]
                    {:old-idx (+ (:old-idx h) (if (added? line) 0 1))
                     :new-idx (+ (:new-idx h) (if (deleted? line) 0 1))
                     :old-lines (if (common? line)
                                  (:old-lines h)
                                  (conj (:old-lines h) [(:old-idx h) line]))
                     :new-lines (if (common? line)
                                  (:new-lines h)
                                  (conj (:new-lines h) [(:new-idx h) line]))
                     })
                  {:old-idx (.getOldOffset hunk)
                   :new-idx (.getNewOffset hunk)
                   :old-lines []
                   :new-lines []}
                  (.getLines hunk)))
            %))
     (map (fn [x] (map #(->> %
                             ((flip select-keys) [:old-lines :new-lines])
                             (map-values (partial map first))) x)))
     ;pprint
     )

(->> "patch/97574c57cf26ace9b8609575bbab66465924fef7_partial.patch"
     resource-path
     slurp
     removed-lines)
