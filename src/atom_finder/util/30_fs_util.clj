(in-ns 'atom-finder.util)

(defn c-files
  "Search directory structure for C-like files"
  [dirname]
  (let [dirfile  (clojure.java.io/file dirname)
        files (file-seq dirfile)
        exts #{"c" "cc" "cpp" "c++" "h" "hh" "hpp" "h++"}]

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
    (str/split-lines (slurp file)))

(defn expand-home [s]
  (if (clojure.string/starts-with? s "~")
    (str/replace-first s "~" (System/getProperty "user.home"))
        s))

(defn pmap-dir-files
  "Apply a function to the AST of every c file in a directory"
  [f dirname]
          (pmap
           (fn [file]
             (let [filename (.getPath file)]
               (try
                 (f filename)
                 (catch Exception e (printf "-- exception parsing file: \"%s\"\n" filename))
                 (catch Error e     (printf "-- error parsing file: \"%s\"\n" filename))
               )
             ))

           (c-files dirname)))

(defn file-ext [file-str]
  "Get the file extension from a filename"
  (some->>
   file-str
   (re-find #"(.*/)?[^/]+\.([^.]+)")
   last))
