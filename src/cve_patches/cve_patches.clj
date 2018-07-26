(ns atom-finder.cve-patches
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.classifier :refer :all]
   [atom-finder.patch :refer :all]
   [atom-finder.atom-stats :refer :all]
   [atom-finder.atom-patch :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.data.csv :as csv]
   [clojure.java.io :as io]
   [clj-jgit.porcelain :as gitp]
   [clj-jgit.querying :as gitq]
   [clj-jgit.internal :as giti]
   [clj-cdt.clj-cdt :refer :all]
   [clj-cdt.writer-util :refer :all]
   [schema.core :as s]
   [swiss.arrows :refer :all]
   )
  (:import
   [atom_finder.classifier Atom]
   [org.eclipse.jgit.lib ObjectReader Repository]
   [org.eclipse.jgit.api Git]
   [org.eclipse.jgit.revwalk RevCommit RevCommitList RevWalk]
   [org.eclipse.jgit.treewalk TreeWalk filter.PathFilter]
   [org.eclipse.cdt.core.dom.ast IASTTranslationUnit IASTNode]
   [org.eclipse.cdt.internal.core.dom.parser ASTNode]
   )
  )


(defn csv-to-maps [filename]
  (with-open [reader (io/reader filename)]
    (let [[header & csv-data] (csv/read-csv reader)]
      (->> csv-data
           (map (%->> (map vector header) (into {})))
           doall))))

(defn repo-name [git-repo-url]
  (nth (re-find #"([^/]+?)(?:.git)?$" git-repo-url) 1))

(def test-cve-repo-urls
  #{"git://anongit.freedesktop.org/NetworkManager/NetworkManager"
    "git://anongit.freedesktop.org/accountsservice"
    "git://anongit.freedesktop.org/cairo"})

(def test-cve-repos
  (->> test-cve-repo-urls
       (map repo-name)
       (map #(vector (repo-name %1) (gitp/load-repo (str "src/cve_patches/repos/" %1))))
       (into {})
       )
  )

(def cve-patches (->> "src/cve_patches/frank_li_cve_patches.csv" csv-to-maps))

(def cve-repo-hashes
  (->> cve-patches
       (map (%-> (get "git_repo_hash")))
       distinct))

;; Find all atoms in cve-patches
'((->> cve-patches
     (filter (%-> (get "git_repo_url") test-cve-repo-urls))
     (take 10)
     (map (fn [patch-map]
            (let [repo (test-cve-repos (repo-name (patch-map "git_repo_url")))
                  rev-commit (find-rev-commit repo (patch-map "git_commit_hash"))]
            (parse-commit-for-atom repo atoms rev-commit))))
     ))
