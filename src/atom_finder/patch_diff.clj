(ns atom-finder.patch-diff
  (:require
   [atom-finder.patch :refer :all]
   [atom-finder.util :refer :all]
   [atom-finder.constants :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as string])
 ;(:import [])
  )

;(->> big-commit-srcs
;     :patch-str
;     parse-diff
;     .getPatches
;     ;removed-lines
;     (mapcat (memfn getHunks))
;     (mapcat hunk-lines-old-lines)
;
;     pprint
;     )
;
;(pprint (map vector
;(->> hunk
;     hunk-lines-old-lines
;     (take 5)
;     ;pprint
;     )
;
;(->> hunk
;     ;hunk-lines-old-lines
;     parallel-hunk-lines
;     (filter (comp not added? :line))
;     (map (juxt :old-idx :line))
;     (take 5)
;     ;pprint
;     ))
;)
