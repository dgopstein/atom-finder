(ns atom-finder.util.FnRangeSet
  (:require
   ;[atom-finder.util :refer :all]
   [clojure.pprint :refer [pprint]]
   )
  (:gen-class ;:name atom_finder.javautil.FnRangeSet
              ;:extends com.google.common.collect.TreeRangeSet
              :implements [com.google.common.collect.RangeSet clojure.lang.IFn]
              :constructors {[com.google.common.collect.RangeSet] []}
              ;:state info
              :init init
              ;:methods [[invoke [Object] Boolean]
              ;          [applyTo [ISeq] Object]]
              ))

(import '(com.google.common.collect Range ImmutableRangeSet TreeRangeSet RangeSet))
;(import '(atom-finder.util2 FnRangeSet))
;(import '(atom_finder.util2 FnRangeSet))

(defn -init
  [range-set]
  [[range-set] (prn "asdf")])

(defn -invoke [this arg] (.contains this arg))
(defn -applyTo [this args] (clojure.lang.AFn/applyToHelper this args))

(defn range-set-constructor
  "Take multiple disjoint ranges and construct a function which
    determines whether a number is included in any of the ranges"
  [range-constructor ranges]; :- [[(s/one s/Int "min") (s/one s/Int "max")]]]
  (let [range-set (doto (TreeRangeSet/create) (.addAll (map range-constructor ranges)))]
    (.build
     (doto (ImmutableRangeSet/builder)
       (.addAll range-set)))))

(def range-set-co (partial range-set-constructor (fn [[a b]] (Range/closedOpen a b))))
(def range-set-cc (partial range-set-constructor (fn [[a b]] (Range/closed a b))))

;(atom-finder.FnRangeSet. (range-set-cc [[1 3] [2 4]]))

(println (.toString
          (.asRanges
 (doto (TreeRangeSet/create)
   (.add (Range/closed 0 4))
   (.add (Range/closed 3 5)))
 )
 ))

; https://stackoverflow.com/questions/9086926/create-a-proxy-for-an-specific-instance-of-an-object-in-clojure
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

((override-delegate [clojure.lang.IFn com.google.common.collect.RangeSet] tree-set
                                        ;(invoke [this] (println msg tree-set))
                   (invoke [this x] (.contains this x))
                   ) 7)

(resolve 'com.google.common.collect.RangeSet)
(map resolve '(com.google.common.collect.RangeSet))
(mapcat #(.getMethods (resolve %)) '(com.google.common.collect.RangeSet))

;; Modifying your example slightly...
'(def realcon (java.sql.DriverManager/getConnection "jdbc:h2:tcp://localhost:9092/test"))
(def con
  (let [msg "FG>"]
    ;(override-delegate '(clojure.lang.IFn com.google.common.collect.RangeSet) tree-set
    (override-delegate (com.google.common.collect.RangeSet) tree-set
                       ;(invoke [this] (println msg tree-set))
                       (contains [this x] true)
                       )
    ))
(.contains con 1)
