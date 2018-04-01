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
            [slp.core.lexing LexerRunner code.JavaLexer]
            [slp.core.modeling ModelRunner CacheModel
             ngram.JMModel mix.InverseMixModel mix.NestedModel]
            ))

(defn pair->vec [p] [(.left p) (.right p)])

(defn init-java-lexer []
  (doto-class LexerRunner
              (setLexer (JavaLexer.))
              (setPerLine false)
              (addSentenceMarkers true)
              (useExtension "java")))

(defn init-modeler [& {:keys [self-test] :or {self-test false}}]
  (doto-class ModelRunner
              (perLine false)
              (setNGramOrder 6)
              (selfTesting self-test)))

(defn ngram-java
  ([train-path] (ngram-java train-path train-path))
  ([train-path test-path]
   (let [train-set (java.io.File. (expand-home train-path))
         test-set (java.io.File. (expand-home test-path))]

     (init-java-lexer)
     (init-modeler :self-test (= train-path test-path))

     (let [model (-<>> (JMModel. (GigaCounter.))
                       (doto <> (ModelRunner/learn train-set))
                       (NestedModel. test-set)
                       (InverseMixModel. <> (CacheModel.))
                       (doto <> (.setDynamic true)))]

       {:model model
        :modeled-files (->> (ModelRunner/model model test-set)
                            .iterator iterator-seq (map pair->vec) (into {}))}
       ))))

(def ngram-res (time-mins (ngram-java "~/atom-finder/src/java")))

(def statistics (->> ngram-res :modeled-files ModelRunner/getStats))

(printf "Modeled %d tokens, average entropy:\t%.4f\n" (.getCount statistics) (.getAverage statistics))

(defn random-split [amount lst]
  (let [shuffled-lst (shuffle lst)]
    (-> shuffled-lst count (* amount) double java.lang.Math/round (split-at shuffled-lst))))

(def nginx-c-files (->> "~/opt/src/atom-finder/nginx" expand-home c-files (random-split 0.9)))

(defn model-files [train-files test-files]
  (init-java-lexer)
  (init-modeler :self-test false)

  (let [model (-<>> (JMModel. (GigaCounter.))
                    (doto <> (ModelRunner/learn train-set))
                    (NestedModel. test-set)
                    (InverseMixModel. <> (CacheModel.))
                    (doto <> (.setDynamic true)))]

    {:model model
     :modeled-files (->> (ModelRunner/model model test-set)
                         .iterator iterator-seq (map pair->vec) (into {}))}
    ))
