(ns atom-finder.constants
  (:require [atom-finder.util :refer :all]
            [clj-jgit.porcelain :as gitp]))

(def gcc-path (expand-home "~/opt/src/gcc"))
(def gcc-repo (gitp/load-repo gcc-path))

(def ag-path (expand-home "~/opt/src/the_silver_searcher"))
(def ag-repo (gitp/load-repo ag-path))

(def root (tu (resource-path "logic-as-control-flow.c")))
;(def big-root (tu (expand-home "~/opt/src/github-top-c/php-src/ext/sqlite3/libsqlite/sqlite3.c")))
(def github-top-c (expand-home "~/opt/src/github-top-c"))
