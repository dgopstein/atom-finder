;; In the current repo, which committers own the atoms

(ns atom-finder.questions.atom-touchers
  (:require
   [atom-finder.atom-patch :refer :all]
   [atom-finder.classifier :refer :all]
   [atom-finder.constants :refer :all]
   [atom-finder.util :refer :all]
   [clojure.pprint :refer :all]
   [clojure.string :as str]
   [clj-jgit.porcelain :as gitp]
   [swiss.arrows :refer :all]
   [schema.core :as s]
   )
  (:import [org.eclipse.cdt.core.dom.ast IASTNode])
  )

(defn atom-finder-repos []
  (->> "~/opt/src/atom-finder"
       expand-home
       list-dirs
       (map gitp/load-repo)
       ))

(s/defn repo-root-dir [repo :- org.eclipse.jgit.api.Git]
  (->> repo .getRepository .getDirectory .getParent))

'((def clang-repo (->>
 (atom-finder-repos)
 first)))



'((->>
   (file-blame clang-repo "examples/PrintFunctionNames/PrintFunctionNames.cpp")
   ;(map (juxt :line (comp :email :author)))
   sort
   (map prn)
))

(defn intra-project-filename [abs-file]
  (str/replace abs-file #".*opt/src/(atom-finder/)?[^/]*/" ""))

(s/defn file-blame :- {s/Int {:line s/Int :author {s/Any s/Any} s/Any s/Any}}
  "For a single file, return the emails of the last developer to touch each line"
  [repo filename]
  (->> filename
       intra-project-filename
       (gitp/git-blame repo)
       (map-indexed (fn [idx blame] [(inc idx) blame]))
       (into {})))

(s/defn atom-touchers ;:- {s/Str {s/Keyword s/Int}} ;; {author {atom count}}
  [repo filename]
  (log-err (str "atom-touchers " filename) {}
    (let [line-authors  (file-blame repo filename)
          atoms         (-> filename parse-file find-all-atoms-non-atoms (dissoc :all-nodes))
          atom-lines    (for [[atm-type nodes] atoms
                              node nodes
                              :let [line (start-line node)]
                              :when line]
                          {:atom atm-type :line line})]

      #_(when (= filename "/Users/dgopstein/opt/src/atom-finder/clang/examples/PrintFunctionNames/PrintFunctionNames.cpp")
          (->> line-authors
               (map #(update-in % [1] (comp :name :author)))
               (sort-by first)
               (map prn)
               dorun)

          (dorun (map prn atom-lines))


          (-> line-authors (get 120) (dissoc :committer) prn)
          )

      {filename
       (->>
        (for [atom-line atom-lines]
          (merge atom-line {:author (-> line-authors (get (:line atom-line))
                                        :author (select-keys [:name :email]))}))
        (group-dissoc :author)
        (map-values (partial frequencies-by :atom))
        )
       }
    )))

(->>
 (atom-finder-repos)
 (mapcat (fn [repo] (pmap-dir-files (partial atom-touchers repo) (repo-root-dir repo))))
 (map (partial map-keys relativize-filename))
 (map prn)
 dorun
 (log-to "tmp/atom-touchers_2018-01-05_01.edn")
 time-mins
 )
