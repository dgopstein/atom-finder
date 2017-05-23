(in-ns 'atom-finder.util)

(def not-empty? (comp not empty?))

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

(defn distinct-by [f col]
  (map first (vals (group-by f col))))

(def map-kv (comp (partial into {}) (partial map)))

(defn map-values-kv [f m]
  (reduce merge (map (fn [[k v]] {k (f k v)}) m)))

(defn map-values [f m] (map-values-kv #(f %2) m))

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

(def flatten1 (partial apply concat))

;; Remove entries in a map based on a predicate
(defn dissoc-by [f m] (->> m (filter (complement f)) (into {})))

(defn avg [seq1] (/ (reduce + seq1) (count seq1)))

(defn min-of [lst]
  "Min with a list argument"
  (if (empty? lst) nil
    (apply min lst)))

(defn max-of [lst]
  "Max with a list argument"
  (if (empty? lst) nil
    (apply max lst)))

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
