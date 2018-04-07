;; Use SLP-core to learn an N-gram model
;; Direct port of SLP-Core/src/main/java/slp/core/example/JavaRunner.java

(ns quark.slp-runner
  (:require [schema.core :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [atom-finder.util :refer :all]
            [swiss.arrows :refer :all]
            )
  (:import
            [slp.core.counting.giga GigaCounter]
            [slp.core.lexing runners.LexerRunner code.JavaLexer]
            [slp.core.modeling runners.ModelRunner CacheModel ngram.JMModel mix.InverseMixModel mix.NestedModel]
            ))

(defn pair->vec [p] [(.left p) (.right p)])
(def pairlist->seq (%->> .iterator iterator-seq (map pair->vec)))
(def pairlist->map (%->> pairlist->seq (into {})))

(defn init-java-lexer []
  (doto (LexerRunner. (JavaLexer.))
    (.setPerLine false)
    (.setSentenceMarkers true)
    (.setExtension "c")))

(defn init-modeler [lexer-runner & {:keys [self-test] :or {self-test false}}]
  (doto (ModelRunner. lexer-runner)
    (.perLine false)
    (.selfTesting self-test)))

(defn ngram-dir
  ([train-path] (ngram-dir train-path train-path))
  ([train-path test-path]
   (let [train-set (java.io.File. (expand-home train-path))
         test-set (java.io.File. (expand-home test-path))
         lexer (init-java-lexer)
         model-runner (init-modeler lexer :self-test (= train-path test-path))]

     (let [model (-<>> (JMModel. 6 (GigaCounter.))
                       (doto <> (#(.learn model-runner % train-set)))  ;;)] model))))
                       (NestedModel. model-runner test-set <>)
                       (InverseMixModel. <> (CacheModel.))
                       (doto <> (.setDynamic true)))]

       {:model model
        :model-runner model-runner
        ;:modeled-files (-> model-runner (.model model test-set) pairlist->map)
        }
       ))))

;(def dir-path (expand-home "~/atom-finder/src/java"))
;(def dir-path (expand-home "~/opt/src/atom-finder/nginx"))
(def dir-path (expand-home "~/opt/src/atom-finder"))
(def dir-file (->> dir-path java.io.File.))

;; 5 mins on all of atom-finder corpus if modeled results are not calculated/returned
(def ngram-res (time-mins (ngram-dir dir-path)))

;(def statistics (-> ngram-res :model-runner (.getStats (-> ngram-res :modeled-files))))

;(printf "Modeled %d tokens, average entropy:\t%.4f\n" (.getCount statistics) (.getAverage statistics))

(defn random-split [amount lst]
  (let [shuffled-lst (shuffle lst)]
    (-> shuffled-lst count (* amount) double java.lang.Math/round (split-at shuffled-lst))))

(def dir-c-files (->> dir-path c-files (random-split 0.9)))

(defn least-common-lines [model]
   (let [lexer (init-java-lexer)
         model-runner (init-modeler lexer :self-test true)]
     (->> dir-file
          (.model model-runner model)
          pairlist->map
          (mapcat (fn [[file problist]]
                    (map-indexed (fn [idx probs] [(-<>> file .getPath (str/replace <> dir-path "")) (inc idx) probs])
                                 problist)))
          (min-n-by 200 (%->> last (apply *))) ;; least probable lines
          ;(min-n-by 50 (%->> last (map #(/ 1 %)) (apply *))) ;; most probable lines
          (remove nil?)
          )))

'((->> ngram-res :model
     least-common-lines
     (map (fn [[file line]]
            ;(println (github-url "nginx" file line))
            (print-line-context 0 (str dir-path "/" file) line :top-border false)
            (println "\n")))
     ;(map (fn [[file line probs]] [(github-url "nginx" file line) probs]))
     ;(take 6)
     ;(map first) (map prn)
     time-mins
     ))

(defn model-all-lines [model]
  (let [lexer (init-java-lexer)
        model-runner (init-modeler lexer :self-test true)]
    (->> dir-file
         (.model model-runner model)
         pairlist->seq
         (mapcat (fn [[file problist]]
                   (map-indexed (fn [idx probs]
                                  {:file (-<>> file .getPath (str/replace <> dir-path ""))
                                   :line (inc idx)
                                   :probability probs})
                                problist)))
         )))

(defmacro ignore-output
  [& block]
  `(with-open [w# (clojure.java.io/writer "/dev/null")]
    (binding [*out* w#, *err* w#]
      ~@block)))

(->> ngram-res :model
     model-all-lines
     (maps-to-csv "atom-finder-token-probabilities.csv")
     ignore-output)

     '((map (fn [[file line]]
            (print-line-context 0 (str dir-path "/" file) line :top-border false)
            (println "\n")))
     time-mins
     )
