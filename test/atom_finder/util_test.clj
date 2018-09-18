(ns atom-finder.util-test
  (:require [clojure.test :refer :all]
            [atom-finder.util :refer :all]
            ;[clj-cdt.clj-cdt :refer :all]
            ;[clj-cdt.writer-util :refer :all]
            )
  (:import
           [org.eclipse.cdt.core.dom.ast IASTExpression IASTLiteralExpression IASTDoStatement]))

(deftest sym-diff-test
  (testing "sym-diff"
    (is (= #{1 4} (sym-diff #{1 2 3} #{2 3 4})))
    (is (= #{1 2 3} (sym-diff #{1 2 3} #{})))
    (is (= #{} (sym-diff #{} #{})))
    (is (= #{} (sym-diff #{1 2 3} #{1 2 3})))
    (is (= #{} (sym-diff #{1 2 3} #{1 2 3} #{1 2 3})))
    (is (= #{1 2 3 4} (sym-diff #{1 2} #{2 3} #{3 4})))
    (is (= #{1 2 4 5} (sym-diff #{1 2 3} #{2 3 4} #{3 4 5})))
    ))

(deftest clj-util-test
  (testing "juxt->>"
    (is (= [2 0] (juxt->> 1 inc dec)))
    (is (= [2 0] (juxt->> 1 (inc) dec)))
    (is (= [3 0] (juxt->> 1 (* 3) dec)))
    (is (= [2 0] (juxt->> 1 (- 3) dec)))
    )

  (testing "flatcat"
    (is (= #{1 2} (set (flatcat [1] [2]))))
    (is (= #{1 2} (set (flatcat  1  [2]))))
    (is (= #{1 2} (set (flatcat [1]  2 ))))
    (is (= #{1 2} (set (flatcat  1   2 ))))
    (is (= #{[1] 2} (set (flatcat [[1]] 2))))
    )

  (testing "flatten1"
    (is (= [1 2 3] (flatten1 [[1 2] [3]])))
    (is (= [1 [2] 3] (flatten1 [[1 [2]] [3]]))))

  (testing "lines-between"
    (let [cases [
                 [[]  [1 1] "a\nb\nc\n"]
                 [["a"] [1 2] "a\nb\nc\n"]
                 [["a" "b"] [1 3] "a\nb\nc\n"]
                 [["a" "b" "c"] [1 4] "a\nb\nc\n"]
                 [["b" "c"] [2 4] "a\nb\nc\n"]
                 [["b" "c"] [2 5] "a\nb\nc\n"]
                 [[] [6 6] "a\nb\nc\n"]
                 ]]

      (doseq [[expected [min max] s] cases]
        (is (= expected (lines-between min max s))
            [expected (str "'" [min max] "' '" s "'")])
      ))
    )

  (testing "auto-map"
    (->> (auto-map x y z) (let [x 1 y 2 z 3]) (= {:x 1 :y 2 :z 3}) is))

  (testing "map-values"
    (is (= {1 1 2 4 3 9} (map-values #(* %1 %1) {1 1 2 2 3 3})))
    (is (= {1 "1a" 2 "2b" 3 "3c"} (map-values-kv #(str %1 %2) {1 \a 2 \b 3 \c})))
    (is (= {} (map-values-kv identity {})))
    (is (= {} (map-values identity {})))
    )

  (testing "map-keys"
    (is (= {1 1 4 2 9 3} (map-keys #(* %1 %1) {1 1 2 2 3 3}))))

  (testing "update-with"
    (-> {:a 1 :b "b" :c {:d [2]}}
        (update-with {[:a] inc
                      [:b] clojure.string/upper-case
                      [:c :d] empty})
        (= {:a 2 :b "B" :c {:d []}})
        is))

  (testing "split-map-by-keys"
    (is (= [{:b 2, :c 3, :d 4} {:a 1, :e 5}]
           (split-map-by-keys {:a 1 :b 2 :c 3 :d 4 :e 5} [:a :e])))
    (is (= [{:b 2, :d 4} {:a 1, :e 5} {:c 3}]
           (split-map-by-keys {:a 1 :b 2 :c 3 :d 4 :e 5} [:a :e] [:c]))))


     (split-map-by-keys {:a 1 :b 2 :c 3 :d 4 :e 5} [:a :e]); => [{:b 2, :c 3, :d 4} {:a 1, :e 5}]
     (split-map-by-keys {:a 1 :b 2 :c 3 :d 4 :e 5} [:a :e] [:c]); => [{:b 2, :d 4} {:a 1, :e 5} {:c 3}]

  (testing "file-ext"
    (let [cases [
                 [nil "gcc/ChangeLog"]
                 ["c" "gcc/random.c"]
                 ["cpp" "gcc/random.cpp"]
                 [nil "gcc/.gitignore"]
                 [nil ".gitignore"]
                 ["txt" "gcc/.gitignore.txt"]
                 ["txt" ".gitignore.txt"]
                 ]]

      (doseq [[expected filename] cases]
        (is (= expected (file-ext filename)) (list "=" expected filename)))
    ))


  (testing "c-file?"
    (let [cases [[true  "abc.c"   ]
                 [true  "abc.h"   ]
                 [true  "abc.c++" ]
                 [true  "abc.cc"  ]
                 [false "abc.j"   ]
                 [false "abc.c.j" ]
                 [false "abc"     ]]]

      (doseq [[expected filename] cases]
        (is (= expected (boolean (c-file? filename))) (list "=" expected filename)))
      )
    )

  (testing "dissoc-by"
    (is (= (dissoc-by #(.contains (first %1) "bad")
                      {"good1" 1, "bad1" 1, "good2" 2, "bad2" 2})
           {"good1" 1, "good2" 2}))
    )

  (testing "max-n-by"
    (is (= [-9 -10 12] (max-n-by 3 #(* % %) [-10 3 5 -5 12 -9 -8])))
    (is (= [-8 -5 3 5 12] (max-n-by 5 identity [-10 3 5 -5 12 -9 -8])))
    (is (= [-9 -10] (max-n-by 2 #(- %) [-10 3 5 -5 12 -9 -8])))
    )


  (testing "min-n-by"
    (is (= [-5 5 3] (min-n-by 3 #(* % %) [-10 3 5 -5 12 -9 -8])))
    (is (= [3 -5 -8 -9 -10] (min-n-by 5 identity [-10 3 5 -5 12 -9 -8])))
    (is (= [5 12] (min-n-by 2 #(- %) [-10 3 5 -5 12 -9 -8])))
    )

  (testing "if-let*"
    (is (= 2 (if-let* [a 1] 2 3)))
    (is (= 3 (if-let* [a nil] 2 3)))
    (is (= 2 (if-let* [a 1 b 4] 2 3)))
    (is (= 3 (if-let* [a nil b 4] 2 3)))
    (is (= 3 (if-let* [a 1 b nil] 2 3)))
    (is (= 3 (if-let* [a nil b nil] 2 3)))
    (is (= 3 (if-let* [a false b true] 2 3)))
    (is (= 2 (if-let* [a true b true c true] 2 3)))
    (is (= 3 (if-let* [a true b true c nil] 2 3)))
    )
  )

(deftest keyword-test
  (testing "join-keywords"
    (is (= :abc_efg (join-keywords "_" [:abc :efg])))
    (is (= :abc_efg (join-keywords "_" ["abc" :efg])))
    (is (= :abc_efg (join-keywords "_" [:abc "efg"])))
    (is (= :abc_efg_hij (join-keywords "_" [:abc "efg" :hij])))
    (is (= :abcefghij (join-keywords [:abc "efg" :hij])))
  ))
