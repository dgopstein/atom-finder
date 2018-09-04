;; Generate a random list of atoms of each type to compare against our
;; original experiments for consistency and representativeness

(ns atom-finder.questions.random-examples
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.constants :refer :all]
   [atom-finder.atom-patch :refer :all]
   [atom-finder.classifier :refer :all]
   [clj-cdt.clj-cdt :refer :all]
   [clj-cdt.writer-util :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.set :as set]
   [schema.core :as s]
   [swiss.arrows :refer :all]
   [clojure.data.csv :as csv]
   [clojure.string :as str]
   )
  (:import
   [org.eclipse.cdt.core.dom.ast IASTNode]
   )
  )

'((def random-c-files
    (->>
     "~/opt/src/atom-finder"
     expand-home
     c-files
     shuffle
     time-mins
     )))

; 2:20 for 100k
(quote
 (def random-atoms
   (->>
                                        ;rare-atom-files (map (partial str "~/opt/src/atom-finder/"))
    random-c-files
    (take 4)
    (pap (constantly (now)))
    (upmapcat
     (fn [filename]
       (with-timeout 120
         (log-err "parsing random example" nil
                  (for [[name atms] (->> filename parse-file find-all-atoms)
                        atm atms]
                    (log-err "serializing random example" nil
                             {:file (.getFilePath atm)
                              :line (start-line atm)
                              :type name
                              ;;:atom atm
                              :github-url (github-url atm)
                              :code-str (write-tree atm)}))))))
    (remove nil?)
    shuffle
    time-mins
    )))


(quote
(->>
 random-atoms
 (group-by :type)
 ;;first last first :github-url)
 (map-values (partial distinct-by (%->> :github-url (re-find #"[^#]*"))))
 (map-values (partial take 40))
 (def grouped-examples)
 time-mins
 )
)

(quote
 (->> grouped-examples
     (map-values (partial map #(str (:github-url %1) "\n\n" (:code-str %1) "\n\n--------------\n")))
     (map (fn [[type atoms]] (spit (str "tmp/atom-examples_reversed_macro-op_literal-enc/" (name type) ".txt") (str/join "\n" atoms))))
     )
 )

;; find examples of only a few types of atoms
'((->> "tmp/atom-counts_2017-11-12_01.edn"
       read-lines
       (remove (%->> :atom-counts ((flip select-values) [:reversed-subscript :macro-operator-precedence :literal-encoding]) (remove zero?) empty?))
       (map :file)
       (def rare-atom-files)
   ))

;; validate that each type of atom has good coverage accross files:
;; for i in `ls`; do echo "$i         "`grep -o 'https[^#]*' $i | sort | uniq -c | wc -l`; done


;; My numbers claim something like 2/3rds of all if-statements omit their curly braces
'((
   ))
