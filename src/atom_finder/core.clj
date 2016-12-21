(ns atom-finder.core
  (:require [atom-finder.classifier :refer :all]
            [atom-finder.count-subtrees :refer :all]
            [atom-finder.constants :refer :all]
            [atom-finder.util :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as string]
            ))

(defn -main
  [& args]

  (if (not (.exists (io/file gcc-path)))
    (errln (str "Many aspects of this project rely on the existence of " gcc-path " and you don't have anything there!"))
    (println "Your environment appears to be set up correctly!"))
)

;(ns-unmap 'atom-finder.core 'all-preprocessor)
