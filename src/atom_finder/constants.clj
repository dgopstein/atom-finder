(ns atom-finder.constants
  (:require [atom-finder.util :refer :all]
            [atom-finder.source-versions :refer :all]
            [atom-finder.atom-patch :refer :all]
            [clj-jgit.porcelain :as gitp]
            [omniconf.core :as cfg]))

; If there's no specified configuration file, default to the travis-ci file
(cfg/populate-from-file (some resource-path ["conf.edn" "travis-ci.edn"]))

(def gcc-path (some->> :gcc-path cfg/get expand-home))
(def  ag-path (some->>  :ag-path cfg/get expand-home))

(def gcc-repo (some->> gcc-path gitp/load-repo))
(def  ag-repo (some->>  ag-path gitp/load-repo))

(def root (parse-resource "logic-as-control-flow.c"))

(def big-root     (some->> :big-root     cfg/get expand-home parse-file))
(def github-top-c (some->> :github-top-c cfg/get expand-home))

(def big-commit-revstr "d4f474145ae66d041b820f4bf118601451baf261")
(def big-commit-file "gcc/config/aarch64/arm_neon.h")
(def big-commit-rev-commit (some-> gcc-repo (find-rev-commit big-commit-revstr)))
(def big-commit-srcs (when (and gcc-repo big-commit-rev-commit) (before-after-data gcc-repo big-commit-rev-commit big-commit-file)))
