(in-ns 'atom-finder.util)

(defmacro log-err [msg ret x]
  `(try ~x
      (catch Exception e# (do (errln (str "-- exception parsing commit: \"" ~msg "\"\n")) ~ret))
      (catch Error e#     (do (errln (str "-- error parsing commit: \""  ~msg "\"\n")) ~ret))))

(defn c-files
  "Search directory structure for C-like files"
  [dirname]
  (let [dirfile  (clojure.java.io/file dirname)
        files (file-seq dirfile)
        exts #{"c" "cc" "cpp" "C" "c++" "h" "hh" "hpp" "h++" "H"}]

    (filter
     (fn [file]
       (and
        (exts (nth (re-find #".*\.([^.]+)" (.getName file)) 1 nil))
        (.isFile file))
       )
     files)))

(defn resource-path
  "Find the path to a resource"
  [filename]
  (some->> filename clojure.java.io/resource clojure.java.io/file .getPath))

(def slurp-resource (comp slurp resource-path))

(defn slurp-lines [file]
  (line-seq (clojure.java.io/reader file)))

(defn expand-home [s]
  (if (clojure.string/starts-with? s "~")
    (str/replace-first s "~" (System/getProperty "user.home"))
        s))

(s/defn pmap-dir-files
  "Apply a function to the AST of every c file in a directory"
  [f dirname]
  (pmap
   (fn [file]
     (let [filename (.getPath file)]
       (log-err (format "file: \"%s\"" filename) nil (f filename))))
   (c-files dirname)))

(defn file-ext [file-str]
  "Get the file extension from a filename"
  (some->>
   file-str
   (re-find #"(.*/)?[^/]+\.([^.]+)")
   last))

(defmacro log-to [filename & stuff]
  `(binding [*out* (clojure.java.io/writer ~filename)]
     ~(cons 'do stuff)))
