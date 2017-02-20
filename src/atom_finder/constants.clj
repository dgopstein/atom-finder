(ns atom-finder.constants
  (:require [atom-finder.util :refer :all]
            [clj-jgit.porcelain :as gitp]
            [omniconf.core :as cfg]))

 (cfg/populate-from-file (resource-path "conf.edn"))
(def gcc-path (expand-home  (cfg/get :gcc-path)))
(def gcc-repo (gitp/load-repo gcc-path))

(def ag-path (expand-home  (cfg/get :ag-path)))
(def ag-repo (gitp/load-repo ag-path))

(def root (tu (resource-path "logic-as-control-flow.c")))
(def big-root (tu (expand-home  (cfg/get :big-root))))
(def github-top-c (expand-home  (cfg/get :github-top-c)))
