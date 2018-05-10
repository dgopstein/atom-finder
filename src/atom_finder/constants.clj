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

(def atom-finder-corpus-path (some->> :atom-finder-corpus-path cfg/get expand-home))

(def github-top-c (some->> :github-top-c cfg/get expand-home))

(def system-include-paths (some->> :system-include-paths cfg/get expand-home))
