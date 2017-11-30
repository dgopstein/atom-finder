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

(def github-repos
  {"gcc"          "gcc-mirror/gcc/blob/2c3133a09ceedead50c2b585ef7f62738ad5c81e/"
   "linux"        "torvalds/linux/blob/e19b205be43d11bff638cad4487008c48d21c103/"
   "clang"     "llvm-mirror/clang/blob/2bcd2d052e5508c12374390e4a2d572988622caf/"
   "freebsd"     "freebsd/freebsd/blob/c2b6ea8fa56ce6aba773d820fbf64a4d3efac9f5/"
   "gecko-dev" "mozilla/gecko-dev/blob/dd47bee6468de7e1221b4d006342ad6b9813d0e5/"
   "vim"                 "vim/vim/blob/5df95ea9ef34b5a898141ddc7134e4a7de713ba5/"
   "webkit"        "WebKit/webkit/blob/e8c73206a09f734bc64f77d6275a727aa2811754/"
   })

(defn github-url
  [found-atom]
  (let [[proj file] (-<>> found-atom filename (str/replace <> #".*opt/src/" "") (re-find #"([^/]+)/(.*)") rest)
        line (start-line found-atom)]
    (str "https://github.com/"
         (github-repos proj)
         file
         "#L"
         line)))

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
