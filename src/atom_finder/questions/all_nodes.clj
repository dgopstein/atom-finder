;; How often is each type of AST node used in our corpus

(ns atom-finder.questions.all-nodes
  (:require
   [atom-finder.constants :refer :all]
   [atom-finder.util :refer :all]
   [atom-finder.questions.question-util :refer :all]
   [clj-cdt.clj-cdt :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as string]
   [swiss.arrows :refer :all]
   [schema.core :as s]
   )
  )

;; List all node counts file-by-file
(defn count-all-nodes-in-project
  [edn-file]
  (println (str (now)))
  (->> atom-finder-corpus-path
       (pmap-dir-c-files
        (fn [file]
          (assoc
           (->> file parse-file potential-atom-nodes (map opname-or-typename) frequencies)
           :file (atom-finder-relative-path file))))
       (map prn)
       dorun
       (log-to edn-file)
       time-mins
       ))

(defn c-files-no-cpp-or-h
  "Search directory structure for C-only files"
  [dirname]
  (->> dirname
       files-in-dir
       (filter #(->> %1 .getName file-ext #{"c"} (and (.isFile %1))))))

(defn summarize-all-nodes [edn-file csv-file]
  (->>
   edn-file
   read-lines
   (map (partial-right dissoc :file))
   (apply merge-with +)
   (sort-by (comp - last))
   (map (partial zipmap [:node-type :count]))
   (maps-to-csv csv-file)
   time-mins
   ))

;; Compare pmap to upmap using atom-finding
'((require '[com.climate.claypoole :as cp]))
'((cp/with-shutdown! [pool (cp/threadpool (+ 2 (available-processors)))]
  (->> [pmap (partial cp/upmap :builtin) (partial cp/upmap pool)]
     (map #(vector % (-<>> "~/opt/src/atom-finder"
                           expand-home
                           c-files-in-dir
                           (take 500)
                           (% (fn [filename]
                                (->> filename
                                     parse-file
                                     find-all-atoms-non-atoms)))
                           dorun
                          time-secs-data)))
     (map prn)
     dorun)))

;; Compare pmap to upmap using Thread/sleep jobs
'((->> [pmap (partial cp/upmap :builtin)]
     (map #(vector % (-<>> (range 300)
                           (% (fn [i]
                                (if (= 0 (mod i 23))
                                  (Thread/sleep 5000))
                                ))
                           dorun
                          time-secs-data)))
     (map prn)))

;; some files are much better parsed as C,
;; but unfortunately our classifiers are not bilingual
'((-<>> "mysql-server/cmd-line-utils/libedit/el.c"
     (str atom-finder-corpus-path "/")
     slurp
     (parse-source <> {:language :c})
     (flatten-tree)
     (group-by problem?)
     (map-values count)
     pprint
     ))

(defn main-all-nodes
  []
  (let [edn-file "tmp/all-node-counts_2018-10-25_disambiguate-minus-arity.edn"
        csv-file "src/analysis/data/all-node-counts_2018-10-25_disambiguate-minus-arity.csv"
        ]
    (count-all-nodes-in-project edn-file)
    (summarize-all-nodes edn-file csv-file)
  ))
