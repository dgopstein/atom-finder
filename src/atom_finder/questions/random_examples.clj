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

(->>
 "~/opt/src/linux"
 expand-home
 c-files
 (take 50000)
 shuffle
 (take 10000)
 (def random-c-files)
 time-mins
 )

(quote
(->>
 random-c-files
 (pap (constantly (now)))
 (map parse-file)
 (mapcat
  (fn [root]
    (for [[name atms] (find-all-atoms root)
          atm atms]
      {:file (str/replace-first (.getFilePath root) #".*linux/" "")
       :line (start-line atm)
       :type name
       :atom atm})))
 shuffle
 (def random-atoms)
 time-mins
 )
)


(quote
(->>
 random-atoms
 (group-by :type)
 (map-values (partial take 100))
 (def grouped-examples)
 time-mins
 )
)

'((->> grouped-examples :preprocessor-in-statement (map (fn [exmpl] ;(assoc (update-in exmpl [:atom] write-tree)
                                                                :github (github-url exmpl))) pprint))

(quote
 (->> grouped-examples
     (map-values (partial map #(update-in % [:atom] write-tree)))
     (map-values (partial map #(str (github-url %1) "\n\n" (:atom %1) "\n\n--------------\n")))
     (map (fn [[type atoms]] (spit (str "tmp/atom-examples/" (name type) ".txt") (str/join "\n" atoms))))
     )
 )
