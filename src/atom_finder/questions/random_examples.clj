;; Generate a random list of atoms of each type to compare against our
;; original experiments for consistency and representativeness

(ns atom-finder.questions.random-examples
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.constants :refer :all]
   [atom-finder.atom-patch :refer :all]
   [atom-finder.classifier :refer :all]
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

'((->>
 "~/opt/src/atom-finder"
 expand-home
 c-files
 shuffle
 (def random-c-files)
 time-mins
 ))

; 2:20 for 100k
(quote
(->>
 random-c-files
 (take 40000)
 (pap (constantly (now)))
 (pmapcat
  (fn [filename]
    (with-timeout 120
      (for [[name atms] (->> filename parse-file find-all-atoms)
            atm atms]
        (log-err "serializing random example" nil
                 {:file (.getFilePath root)
                  :line (start-line atm)
                  :type name
                  ;;:atom atm
                  :github-url (github-url atm)
                  :code-str (write-tree atm)})))))
 (remove nil?)
 shuffle
 (def random-atoms)
 time-mins
 )
)


(quote
(->>
 random-atoms
 (group-by :type)
 (map-values (partial take 40))
 (def grouped-examples)
 time-mins
 )
)

(quote
 (->> grouped-examples
     (map-values (partial map #(str (:github-url %1) "\n\n" (:code-str %1) "\n\n--------------\n")))
     (map (fn [[type atoms]] (spit (str "tmp/atom-examples/" (name type) ".txt") (str/join "\n" atoms))))
     )
 )
