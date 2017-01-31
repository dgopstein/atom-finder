(ns atom-finder.constants
  (:require [atom-finder.util :refer :all]
            [clj-jgit.porcelain :as gitp]
            [clojure.data.json :as json]))

(def conf-data (json/read-str (slurp (resource-path "conf.json")):key-fn keyword))

(def gcc-path (expand-home (get conf-data :gcc-path)))
(def gcc-repo (gitp/load-repo gcc-path))

(def ag-path (expand-home (get conf-data :ag-path)))
(def ag-repo (gitp/load-repo ag-path))

(def root (tu (resource-path "logic-as-control-flow.c")))
(def big-root (tu (expand-home (get conf-data :big-root))))
(def github-top-c (expand-home (get conf-data :github-top-c)))
