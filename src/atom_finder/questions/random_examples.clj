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
   [clojure.data.csv :as csv]
   [clojure.string :as str]
   )
  (:import
   [org.eclipse.cdt.core.dom.ast IASTNode]
   )
  )

(defn github-url
  [found-atom]
    (str "https://github.com/"
      (if (str/starts-with? (str (:file found-atom)) "gcc")
        "gcc-mirror/gcc/blob/2c3133a09ceedead50c2b585ef7f62738ad5c81e/"
        "torvalds/linux/blob/e19b205be43d11bff638cad4487008c48d21c103/")
      (str/replace-first (:file found-atom) #"[^/]*/" "")
      "#L"
      (:line found-atom)))

(->>
 "~/opt/src/linux"
 expand-home
 c-files
 ;(take 100000)
 shuffle
 (take 1000)
 (def random-c-files)
 time-mins
 )

(quote
(->>
 random-c-files
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

(quote
 (->> grouped-examples
     :macro-operator-precedence
     first
     :atom
     class)
     ;str)
)

(quote
 (->> grouped-examples
     (map-values (partial map #(update-in % [:atom] safe-write-ast)))
     (map-values (partial map #(str (github-url %1) "\n\n" (:atom %1) "\n\n--------------\n")))
     (map (fn [[type atoms]] (spit (str "tmp/atom-examples/" (name type) ".txt") (str/join "\n" atoms))))
     )
 )
