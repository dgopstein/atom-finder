(ns atom-finder.constants
  (:require [atom-finder.util :refer :all]
            [clj-jgit.porcelain :as gitp]
            [omniconf.core :as cfg]))

(cfg/populate-from-file (resource-path "conf.edn"))


(def gcc-path (some->> :gcc-path cfg/get expand-home))
(def ag-path  (some->> :ag-path  cfg/get expand-home))

(def gcc-repo (some->> gcc-path gitp/load-repo))
(def ag-repo  (some->> ag-path  gitp/load-repo))

(def root (tu (resource-path "logic-as-control-flow.c")))

(def big-root     (some->> :big-root     cfg/get expand-home tu))
(def github-top-c (some->> :github-top-c cfg/get expand-home))
