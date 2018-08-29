;; Search for atoms in security vulnerability fix patches in the data set from
;; A Large-Scale Empirical Study of Security Patches - Frank Li, Vern Paxson
;; Compare the rates of atom removal to atom addition

(ns atom-finder.questions.cve-patches
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.classifier :refer :all]
   [atom-finder.patch :refer :all]
   [atom-finder.atom-stats :refer :all]
   [atom-finder.atom-patch :refer :all]
   [atom-finder.commits-added-removed :refer :all]
   [atom-finder.questions.question-util :refer :all]
   [clojure.java.shell :as shell]
   [clojure.string :as str]
   [clojure.pprint :refer [pprint]]
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

(defn repo-name [git-repo-url]
  (nth (re-find #"([^/]+?)(?:.git)?$" git-repo-url) 1))

(def cve-patches (->> "src/data/frank_li_cve_patches_sorted.csv" csv-to-maps))

(defn safe-load-repo [path]
  (log-err (str "Error accessing repo " path) nil
  (gitp/load-repo path)))

(def cve-diff-archive "src/data/frank_li_cve_patch_data_c-files-only.tar.xz")
(def cve-diff-path "src/data/frank_li_cve_patch_data_c-files-only/")
(defn extract-cve-diffs []
  (when (not (path-exists? cve-diff-path))
    (println "Extracting archive " cve-diff-archive "to" cve-diff-path "...")
    (prn
     (shell/sh "tar" "-xf" cve-diff-archive "-C" (-> cve-diff-path (str/replace #"[^/]*.$" ""))))))

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
(defn atoms-in-franks-patches [edn-file]
  (->> cve-patches
     ;;(drop-while #(not= (get % "cve_ids") "CVE-2012-3552")) rest
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
                              :when (some? srcs)
                              :when (-> srcs :file c-file?)]
                          (merge (added-removed-atoms-count srcs)
                                 {:git-repo-hash (srcs "git_repo_hash")
                                  :git-repo-url (srcs "git_repo_url")
                                  :cve-ids (srcs "cve_ids")
                                  }))))
     )
     (remove nil?)
     (map prn)
     dorun
     (log-to edn-file)
     time-mins
     ))

(defn summarize-atoms-in-franks-patches
  [edn-file csv-prefix]
  (->> edn-file
       read-lines
       (filter :added-non-atoms)
       (map #(merge % {:n-added   (+ (:added-non-atoms %)   (sum (vals (:added-atoms %))))
                       :n-removed (+ (:removed-non-atoms %) (sum (vals (:removed-atoms %))))}))
       (map (partial-right split-map-by-keys [:added-atoms] [:removed-atoms]))
       (map (fn [[common added removed]] [(merge common (:added-atoms added)) (merge common (:removed-atoms removed))]))
       transpose
       ((fn [[addeds removeds]]
          (maps-to-csv (str csv-prefix "_added.csv") {:headers (-> addeds first keys (concat (map :name atoms)))} addeds)
          (maps-to-csv (str csv-prefix "_removed.csv") {:headers (-> removeds first keys (concat (map :name atoms)))} removeds)
               ))
       ))



(defn main-atom-counts []
  (let [edn-file "tmp/cve-patch-atoms-added-removed-from-patches_2018-08-29_fixed-omitted-curly-brace.edn"
        csv-prefix "src/analysis/data/cve-patch-atoms_2018-08-29_fixed-omitted-curly-brace"]
    (prn (str (now)))
    (extract-cve-diffs)
    (println "Searching for atoms in security patches...")
    (atoms-in-franks-patches edn-file)
    (summarize-atoms-in-franks-patches edn-file csv-prefix)
    ))
