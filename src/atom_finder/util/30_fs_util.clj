(in-ns 'atom-finder.util)

(defmacro log-err [msg ret & x]
  `(try ~@x
      (catch Exception e# (do (errln (str "-- exception: \"" ~msg "\"\n")) ~ret))
      (catch Error e#     (do (errln (str "-- error: \""  ~msg "\"\n")) ~ret)))
  #_x
  )

(defn file-ext [file-str]
  "Get the file extension from a filename"
  (some->>
   file-str
   (re-find #"(.*/)?[^/]+\.([^.]+)")
   last))

(defmulti c-file? class)
(defmethod c-file? String [filename]
  ;; All extensions that are
  ;; * Known to be C/C++ code - e.g. .c++/.h++, and not .vert/.frag
  ;; * Are common - having more than 100k nodes in this corpus
  ;; * Are parsable - having an average problem rate of <0.1
  (->> filename file-ext #{"c" "cc" "cpp" "cxx" "c++" "C" "h" "hh" "hpp" "h++" "H" "ipp"}))
(defmethod c-file? java.io.File [file]
  (and (.isFile file)
       (c-file? (.getName file))))

(defn list-dirs
  [path]
  (->>
   path
   clojure.java.io/file
   .listFiles
   (filter #(.isDirectory %))))

(defn files-in-dir
  [dirname]
  (->> dirname clojure.java.io/file file-seq))

(defn c-files
  "Search directory structure for C-like files"
  [dirname]
  (filter (fn [file] (and (c-file? (.getName file)) (.isFile file)))
          (files-in-dir dirname)))

(defn resource-path
  "Find the path to a resource"
  [filename]
  (some->> filename clojure.java.io/resource clojure.java.io/file .getPath))

(def slurp-resource (comp slurp resource-path))

(defn slurp-lines [file]
  (line-seq (clojure.java.io/reader file)))

(def home-dir (System/getProperty "user.home"))

(defn expand-home [s]
  (if (clojure.string/starts-with? s "~")
    (str/replace-first s "~" home-dir)
    s))

(defn remove-home [s]
  (if (clojure.string/starts-with? s home-dir)
    (str/replace-first s home-dir "")
    s))

;; Should we use claypoole.lazy instead?
;; https://github.com/TheClimateCorporation/claypoole#lazy
(require '[com.climate.claypoole :as cp])
(s/defn pmap-dir-c-files
  "Apply a function to the filename of every c file in a directory"
  [f dirname]
  (cp/upmap :builtin
   (fn [file]
     (let [filename (.getPath file)]
       (log-err (format "pmap-dir-file: \"%s\"" filename) nil
                (f filename))))
   (c-files dirname)))

(defmacro log-to [filename & stuff]
  `(binding [*out* (clojure.java.io/writer ~filename)]
     ~(cons 'do stuff)))

(defn path-exists? [filename]
  (->> filename java.io.File. .exists))
(defn file-exists? [filename]
  (let [file (java.io.File. filename)]
    (and (.exists file) (not (.isDirectory file)))))
