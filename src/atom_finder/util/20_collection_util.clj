(in-ns 'atom-finder.util)

(def not-empty? (comp not empty?))

(defmacro auto-map
  "create a map with keys named after the arguments of this function"
  [& args]
  `(zipmap (list ~@(map keyword args)) (list ~@args)))

(defn sym-diff
  "Set symmetric difference - the opposite of the intersection"
  [& args]
  (clojure.set/difference
   (apply clojure.set/union args)
   (apply clojure.set/intersection args)))

(def any-pred? (comp boolean some))
(defn exists?
  ([lst] (any-pred? true? lst))
  ([pred lst] (any-pred? pred lst)))

(defn frequencies-by [f coll]
  (->> coll (map f) frequencies))

;; https://crossclj.info/ns/prismatic/plumbing/0.5.4/plumbing.core.cljs.html#_distinct-by
(defn distinct-by
  "Returns elements of xs which return unique
   values according to f. If multiple elements of xs return the same
   value under f, the first is returned"
  [f xs]
  (let [s (atom #{})]
    (for [x xs
          :let [id (f x)]
          :when (not (contains? @s id))]
      (do (swap! s conj id)
          x))))

(defn map-kv [f m]
  (->> m (map (fn [[k v]] (f k v))) (into {})))

(defn map-values-kv [f m]
  (map-kv (fn [k v] {k (f k v)}) m))

(defn map-values [f m] (map-values-kv #(f %2) m))

(defn map-keys [f m] (map-kv (fn [k v] [(f k) v]) m))

; https://ideone.com/fork/P2876
(def mapcat-indexed
  "like mapcat, but expects function of 2 arguments, where first argument is index of sequence element"
  (comp (partial apply concat) map-indexed))

(defn values-at [m keys]
  (map m keys))

(s/defn update-with :- {s/Any s/Any}
  "Update several values in a map based on functions"
  [m-orig updaters :- {s/Any (s/=> s/Any s/Any)}]
  (reduce (fn [m [keys f]] (update-in m keys f)) m-orig updaters))

(def transpose (partial apply map vector))

(defn strict-get
  "Lookup value in collection and throw exception if it doesn't exist"
  [m k]
  (if-let [[k v] (find m k)]
    v
        (throw (Exception. (str "Key Not Found " k)))))

; http://stackoverflow.com/questions/43213573/get-in-for-lists/43214175#43214175
(defn get-nth-in [init ks]
  (reduce
   (fn [a k]
     (if (associative? a)
       (get a k)
       (nth a k)))
   init ks))

; https://crossclj.info/ns/logicadb/0.1.0/com.kurogitsune.logicadb.core.html#_safe-nth
(defn safe-nth [x n] (try (nth x n) (catch Exception e nil)))

;; http://blog.jayfields.com/2011/01/clojure-select-keys-select-values-and.html
(def select-values (comp vals select-keys))

(def flatten1 (partial apply concat))

;; Remove entries in a map based on a predicate
(defn dissoc-by [f m] (->> m (filter (complement f)) (into {})))

(defn avg [seq1] (/ (reduce + seq1) (count seq1)))

(defn min-of
  "Min with a list argument"
  [lst]
  (if (empty? lst) nil
    (apply min lst)))

(defn max-of
  "Max with a list argument"
  [lst]
  (if (empty? lst) nil
    (apply max lst)))

(def max-by (partial apply max-key))
(def min-by (partial apply min-key))

(defn max-n-by
  "Find the n largest elements by f"
  [n f lst]
  (let [not-smallest (fn [acc x] (->> [(f x) x] (conj acc) (sort-by first) rest))]
    (reduce not-smallest (repeat n [0 nil]) lst)))

(defn group-dissoc
  "Group a list of maps by a key, then dissoc that key"
  [key coll]
  (->> coll (group-by key) (map-values (partial map #(dissoc % key)))))

(defn find-after
  "Take the element after the specified one"
  [coll elem]
  (->> (map vector coll (rest coll))
       (filter #(= elem (first %)))
       first
       last))

(def find-first (comp first (partial filter)))

; https://github.com/mikera/clojure-utils/blob/master/src/main/clojure/mikera/cljutils/loops.clj
(defmacro doseq-indexed
    "loops over a set of values, binding index-sym to the 0-based index of each value"
  ([[val-sym values index-sym] & code]
   `(loop [vals# (seq ~values)
           ~index-sym (long 0)]
      (if vals#
        (let [~val-sym (first vals#)]
          ~@code
          (recur (next vals#) (inc ~index-sym)))
               nil))))

; https://github.com/clojure/clojure-contrib/blob/7f2b012cb679d0ad19f8949c95b7ef479fe1ff22/src/main/clojure/clojure/contrib/seq_utils.clj#L49
(defn separate
    "Returns a vector:
   [ (filter f s), (filter (complement f) s) ]"
  [f s]
    [(filter f s) (filter (complement f) s)])

(s/defn set-difference-by
  "set-difference after applying a function to each element"
  [f s1 s2]
  (let [m1 (zipmap (map f s1) s1)]
    (vals (apply dissoc m1 (map f s2)))))
'(set-difference-by #(* %1 %1) [1 2 3] [-1 2 -4])

(defn split-map-by-keys [m & keyses]
  (cons (apply dissoc m (apply concat keyses))
        (map (partial select-keys m) keyses)))

(def sum (partial reduce +))
