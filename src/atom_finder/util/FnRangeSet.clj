(in-ns 'atom-finder.util)
(import '(com.google.common.collect Range ImmutableRangeSet TreeRangeSet RangeSet))

; https://stackoverflow.com/questions/9086926/create-a-proxy-for-an-specific-instance-of-an-object-in-clojure
; Facility to create a new object that delegates all its methods to an exist object
; Let's us make a java object call-able as an IFn
(defmacro override-delegate
  [types delegate & body]
  (let [d (gensym)
        overrides (group-by first body)
        methods (for [m (mapcat #(.getMethods (resolve %)) types)
                      :let [f (-> (.getName m)
                                  symbol
                                  (with-meta {:tag (-> m .getReturnType .getName)}))]
                      :when (not (overrides f))
                      :let [args (for [t (.getParameterTypes m)]
                                   (with-meta (gensym) {:tag (.getName t)}))]]
                  (list f (vec (conj args 'this))
                        `(. ~d ~f ~@(map #(with-meta % nil) args))))]
    `(let [~d ~delegate]
       (reify ~@types ~@body ~@methods))))

;; Add the ability to treat a range-set like a function, like a regular clojure set
(defn make-ifn
  [tree-set]
  (override-delegate
   [clojure.lang.IFn com.google.common.collect.RangeSet] tree-set
     (invoke [this arg] (.contains this arg))
     (applyTo [this args] (clojure.lang.AFn/applyToHelper this args))))

(defn range-set-constructor
  "Take multiple disjoint ranges and construct a function which
    determines whether a number is included in any of the ranges"
  [range-constructor ranges]; :- [[(s/one s/Int "min") (s/one s/Int "max")]]]
  (ImmutableRangeSet/unionOf (map range-constructor ranges)))

(defn range-set-co [ranges] (make-ifn (range-set-constructor (fn [[a b]] (Range/closedOpen a b)) ranges)))
(defn range-set-cc [ranges] (make-ifn (range-set-constructor (fn [[a b]] (Range/closed a b)) ranges)))
