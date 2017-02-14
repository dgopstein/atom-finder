(ns atom-finder.core
  (:require [atom-finder.classifier :refer :all]
            [atom-finder.count-subtrees :refer :all]
            [atom-finder.util :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [omniconf.core :as cfg]
            ))

(defn -main
  [& args]

  (cfg/populate-from-file (resource-path "conf.edn"))
  (if (not (.exists (io/file (cfg/get :jake-dev-conf :gcc-path))))
    (errln (str "Many aspects of this project rely on the existence of " (cfg/get :jake-dev-conf :gcc-path) " and you don't have anything there!"))
    (println "Your environment appears to be set up correctly!"))
)

;(ns-unmap 'atom-finder.core 'contains-location?)
