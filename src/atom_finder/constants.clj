(ns atom-finder.constants
  (:require [atom-finder.util :refer :all]
            [clj-jgit.porcelain :as gitp]
            [omniconf.core :as cfg]))

; If there's no specified configuration file, default to the travis-ci file
(cfg/populate-from-file (some resource-path ["conf.edn" "travis-ci.edn"]))

(def    ag-path (some->>    :ag-path cfg/get expand-home))
(def   gcc-path (some->>   :gcc-path cfg/get expand-home))
(def linux-path (some->> :linux-path cfg/get expand-home))

(def    ag-repo (some->>    ag-path gitp/load-repo))
(def   gcc-repo (some->>   gcc-path gitp/load-repo))
(def linux-repo (some->> linux-path gitp/load-repo))

(def root (parse-resource "logic-as-control-flow.c"))

(def big-root     (when gcc-path (->> "/gcc/config/i386/i386.c" (str gcc-path) parse-file)))
(def github-top-c (some->> :github-top-c cfg/get expand-home))
