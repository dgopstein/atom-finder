(ns atom-finder.cve-patches
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.classifier :refer :all]
   [atom-finder.patch :refer :all]
   [atom-finder.atom-stats :refer :all]
   [atom-finder.atom-patch :refer :all]
   [atom-finder.commits-added-removed :refer :all]
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

'(def test-cve-repo-urls
  #{"git://anongit.freedesktop.org/NetworkManager/NetworkManager"
    "git://anongit.freedesktop.org/accountsservice"
    "git://anongit.freedesktop.org/cairo"})

(def cve-patches (->> "src/cve_patches/frank_li_cve_patches_sorted.csv" csv-to-maps))

(defn safe-load-repo [path]
  (log-err (str "Error accessing repo " path) nil
  (gitp/load-repo path)))

(def cve-repos
  (->> cve-patches
       (map #(get % "git_repo_hash"))
       (map #(vector %1 (safe-load-repo (str "/mnt/external/cve_repos/" %1))))
       (remove (comp nil? last))
       (into {})
       )
  )

(def cve-repo-hashes
  (->> cve-patches
       (map (%-> (get "git_repo_hash")))
       distinct))

(def c-files-in-dir (%->> clojure.java.io/file file-seq (filter c-file?)))

(def cve-diff-path "src/cve_patches/patch_data_sharing/")
(defn remove-cve-path-prefix [path]
  (clojure.string/replace path (re-pattern (str ".*" cve-diff-path "[^/]*/")) ""))

(defn before-after-data-cve
  "analagous to before-after-data except doesn't need a repo"
  [diff-file before-file after-file]
  (let [file-name (->> after-file .getAbsolutePath remove-cve-path-prefix)
        patch-str (slurp diff-file)
        source-before (slurp before-file)
        source-after (slurp after-file)]
  (merge
     {:file file-name
      :patch-str patch-str
      }

     (build-srcs file-name source-before source-after))))

(defn commit-files-before-after-cve
  "analagous to commit-files-before-after except doesn't need a repo"
  [diff-files before-files after-files]
  (->> (map vector diff-files before-files after-files)
       (map (partial apply before-after-data-cve))
  ))

;; Find all atoms in Franks zip of cve patches
'((->> cve-patches
     (drop-while #(not= (get % "cve_ids") "CVE-2012-3552")) rest
     (map #(assoc % :patch-path (str cve-diff-path (get % "git_repo_hash") "_" (get % "git_commit_hash"))))
     (map #(merge %
             {:diff-files (-> % :patch-path (str "/diff") c-files-in-dir)
              :before-files (-> % :patch-path (str "/parent") c-files-in-dir)
              :after-files (-> % :patch-path (str "/commit") c-files-in-dir)
             }))

     (map #(map (fn [srcs] (merge % {:rev-commit (% "git_commit_hash")
                                     :rev-str    (% "git_commit_hash")
                                     :patch (atom-finder.zutubi/parse-unified-diff (srcs :patch-str))}
                                  srcs))
                  (commit-files-before-after-cve (% :diff-files) (% :before-files) (% :after-files))))
     (mapcat (fn [files-srcs]
               (log-err (str "cve-patches: " files-srcs) nil
                        (for [srcs files-srcs
                              :when (some? srcs)]
                          (merge (added-removed-atoms-count srcs)
                                 {:git-repo-hash (srcs "git_repo_hash")
                                  :git-repo-url (srcs "git_repo_url")
                                  :cve-ids (srcs "cve_ids")
                                  }))))
     )
     (remove nil?)
     (map prn)
     dorun
     (log-to "tmp/cve-patch-atoms-added-removed-from-patches_2018-08-08_3.edn")
     time-mins
     ))

;; Find all atoms in cve-patches from manually gathered repos
'((->> cve-patches
     (filter #(get cve-repos (get % "git_repo_hash"))) ; we couldn't clone some repos
     ;(drop-while #(not= (get % "cve_ids") "CVE-2013-1788")) rest
     (take 10)
     (mapcat (fn [patch-map]
            (log-err (str "cve-patches: " patch-map) nil
                     (let [repo (cve-repos (patch-map "git_repo_hash"))
                           rev-commit (find-rev-commit repo (patch-map "git_commit_hash"))
                           files-srcs (commit-files-before-after repo rev-commit)
                           ]
                       (for [srcs files-srcs
                             :when (some? srcs)]
                         (merge (added-removed-atoms-count srcs)
                                {:git-repo-hash (patch-map "git_repo_hash")
                                 :git-repo-url (patch-map "git_repo_url")
                                 :cve-ids (patch-map "cve_ids")
                                 }))))))
     (remove nil?)
     (map prn)
     dorun
     ;;(log-to "tmp/cve-patch-atoms-added-removed_2018-07-27.edn")
     time-mins
     ))

'((->> "tmp/cve-patch-atoms-added-removed-from-patches_2018-08-08_complete.edn"
       read-lines
       (filter :added-non-atoms)
       (map #(merge % {:n-added   (+ (:added-non-atoms %)   (sum (vals (:added-atoms %))))
                       :n-removed (+ (:removed-non-atoms %) (sum (vals (:removed-atoms %))))}))
       (map (partial-right split-map-by-keys [:added-atoms] [:removed-atoms]))
       (map (fn [[common added removed]] [(merge common (:added-atoms added)) (merge common (:removed-atoms removed))]))
       transpose
       ((fn [[addeds removeds]]
          (maps-to-csv "src/analysis/data/cve-patch-atoms_2018-08-08_added.csv" {:headers (-> addeds first keys (concat (map :name atoms)))} addeds)
          (maps-to-csv "src/analysis/data/cve-patch-atoms_2018-08-08_removed.csv" {:headers (-> removeds first keys (concat (map :name atoms)))} removeds)
               ))
       ))

