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

  (print-atoms-in-dir
   (expand-home "~/opt/src/redis")
   ;(map atom-lookup [:post-increment])
   [{:name :post-*crement-not-atom, :finder
     (atom-finder.classifier/default-finder
      (fn [node] (and (post-*crement? node)
                      (not (post-*crement-atom? node)))))}]
   )
)

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


(defn found-atom-source
  "Return the source code for an atom found in a file"
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

;(def psgl (read-string (slurp (clojure.java.io/file (ClassLoader/getSystemResource "data/preprocessor_subscript_gcc_linux_2017-02-13.edn")))))
;(pprint (sum-found-atoms (dedupe-preprocessors psgl)))
; (found-atom-source :reversed-subscript psgl)
(def psgl (read-string (slurp (clojure.java.io/file (ClassLoader/getSystemResource "data/redis_post_increment_2017-03-04.edn")))))
(def psgl (read-string (slurp (clojure.java.io/file (ClassLoader/getSystemResource "data/redis_not_post_increment_2017-03-04.edn")))))
(pprint (sum-found-atoms psgl))
(found-atom-source :post-increment psgl)
(found-atom-source :post-*crement-not-atom psgl)

gcc-repo
