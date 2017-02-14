(ns atom-finder.core
  (:require [atom-finder.classifier :refer :all]
            [atom-finder.count-subtrees :refer :all]
            [atom-finder.constants :refer :all]
            [atom-finder.util :refer :all]
            [atom-finder.atoms-in-dir :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            ))

(defn -main
  [& args]

  (if (not (.exists (io/file gcc-path)))
    (errln (str "Many aspects of this project rely on the existence of " gcc-path " and you don't have anything there!")))

  (print-atoms-in-dir
   (expand-home "~/opt/src/redis")
   (map atom-lookup [:preprocessor-in-statement :reversed-subscript]))
)

;(ns-unmap 'atom-finder.core 'contains-location?)

(def psgl (read-string (slurp (clojure.java.io/file (ClassLoader/getSystemResource "data/preprocessor_subscript_gcc_linux_2017-02-13.edn")))))

(defn dedupe-preprocessors
  [results]
(->> results
     ;(take 3)
     (map #(update-in % [1 :preprocessor-in-statement] distinct))
     )
  )

(defn sum-found-atoms
  [results]
  (->> results
       ;(take 300)
       (map last)
       (map (partial map-values count))
       (reduce (partial merge-with +))
       )
  )

;(pprint (sum-found-atoms (dedupe-preprocessors psgl)))

(defn found-atom-source
  [atom-name results]
  (->> results
       (filter #(not-empty (atom-name (last %))))
       (map (fn [[filename hash]]
              [filename
              (map #(vector %1 (nth (slurp-lines (expand-home filename)) (dec %1)))
                   (atom-name hash))]))

       pprint
       )
  )

; (found-atom-source :reversed-subscript psgl)
(def vsx_preempt (slurp-lines (expand-home "~/opt/src/linux/tools/testing/selftests/powerpc/math/vsx_preempt.c")))
;(pprint (map vector (range) vsx_preempt))
