(in-ns 'atom-finder.util)

;; https://gist.github.com/jizhang/4325757
(defn md5 [^String s]
  (let [algorithm (java.security.MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (java.math.BigInteger. 1 raw))))

(def github-author
  {"linux"        "torvalds"
   "freebsd"      "freebsd"
   "gecko-dev"    "mozilla"
   "webkit"       "WebKit"
   "gcc"          "gcc-mirror"
   "clang"        "llvm-mirror"
   "mongo"        "mongodb"
   "mysql-server" "mysql"
   "subversion"   "apache"
   "git"          "git"
   "emacs"        "emacs-mirror"
   "vim"          "vim"
   "httpd"        "apache"
   "nginx"        "nginx"
   })

(defn github-repos [proj] (str (github-author proj) "/" proj))

;; for i in `ls`; do cd $i; echo -n "$i   "; echo `git rev-parse HEAD`; cd ..; done
(def repo-rev-strs
  {
   "clang"         "2bcd2d052e5508c12374390e4a2d572988622caf"
   "emacs"         "cb73c70180f57f3fb99fae3aaefbacf0a61cea3f"
   "freebsd"       "c2b6ea8fa56ce6aba773d820fbf64a4d3efac9f5"
   "gcc"           "2201c33012d4c6dc522ddbfa97f5aa95a209e24d"
   "gecko-dev"     "dd47bee6468de7e1221b4d006342ad6b9813d0e5"
   "git"           "ba78f398be65e941b93276680f68a81075716472"
   "httpd"         "6fe234860d15c797f34bc9c0e290fd23a73233c7"
   "linux"         "f34157878d3b17641ad2366988600c23c89d98b2"
   "mongo"         "67f735e6705091659e2a8cf46a9285f09bcf749a"
   "mysql-server"  "0138556a55168da12eaf0bc3038947148d6b0863"
   "nginx"         "9cb9ce78b1d81ccdbcd123ccc3dab295b836a174"
   "subversion"    "0a73cab17bd4114d1ce96dc5b89b0e44ec0fd5d1"
   "vim"           "6ce650480844bfaa5410874416b4a2e15f40b870"
   "webkit"        "e8c73206a09f734bc64f77d6275a727aa2811754"
   })

(defn relativize-filename
  [filename]
  (-> filename (str/replace #".*opt/src/(atom-finder/)?" "")))

(defn github-url
  ([author proj rev-str file line]
   (str "https://github.com/" author "/" proj "/blob/" rev-str "/" file "#L" line))
  ([found-atom]
   (let [[proj file] (->> found-atom filename relativize-filename (re-find #"([^/]+)/(.*)") rest)
         line (start-line found-atom)
         author (github-author proj)
         rev-str (repo-rev-strs proj)]
     (github-url author proj rev-str file line))))

(defn github-commit-url
  ([author proj rev-str file line L-or-R]
   (str "https://github.com/" author "/" proj "/commit/" rev-str "#diff-" (md5 file) L-or-R line)))


