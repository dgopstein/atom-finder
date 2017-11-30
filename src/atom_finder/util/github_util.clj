(in-ns 'atom-finder.util)

;; https://gist.github.com/jizhang/4325757
(defn md5 [^String s]
  (let [algorithm (java.security.MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (java.math.BigInteger. 1 raw))))

(def github-author
  {"gcc"       "gcc-mirror"
   "linux"     "torvalds"
   "clang"     "llvm-mirror"
   "freebsd"   "freebsd"
   "gecko-dev" "mozilla"
   "vim"       "vim"
   "webkit"    "WebKit"
   })

(defn github-repos [proj] (str (github-author proj) "/" proj))

(def repo-rev-strs
  {"gcc"       "2c3133a09ceedead50c2b585ef7f62738ad5c81e"
   "linux"     "e19b205be43d11bff638cad4487008c48d21c103"
   "clang"     "2bcd2d052e5508c12374390e4a2d572988622caf"
   "freebsd"   "c2b6ea8fa56ce6aba773d820fbf64a4d3efac9f5"
   "gecko-dev" "dd47bee6468de7e1221b4d006342ad6b9813d0e5"
   "vim"       "5df95ea9ef34b5a898141ddc7134e4a7de713ba5"
   "webkit"    "e8c73206a09f734bc64f77d6275a727aa2811754"
   })

(defn relativize-filename
  [filename]
  (-> filename (str/replace #".*opt/src/" "")))

(defn github-url
  ([author proj rev-str file line]
   (str "https://github.com/" author "/" proj "/" rev-str "/" file "#L" line))
  ([found-atom]
   (let [[proj file] (->> found-atom filename relativize-filename (re-find #"([^/]+)/(.*)") rest)
         line (start-line found-atom)
         author (github-author proj)
         rev-str (repo-rev-strs proj)]
     (github-url author proj rev-str file line))))

(defn github-commit-url
  ([author proj rev-str file line L-or-R]
   (str "https://github.com/" author "/" proj "/commit/" rev-str "#diff-" (md5 file) L-or-R line)))

(github-commit-url (github-author "clang") "clang" "890783b5e028a061bb51a4a978e8114b55073932" "test/clang-rename/TemplatedClassFunction.cpp" 9 "L")


