;; Use SLP-core to learn an N-gram model
;; Direct port of SLP-Core/src/main/java/slp/core/example/JavaRunner.java

(ns quark.slp-runner
  (:require [schema.core :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [atom-finder.util :refer :all]
            )
  (:import
            [slp.core.counting.giga GigaCounter]
            [slp.core.lexing LexerRunner code.JavaLexer]
            [slp.core.modeling ModelRunner CacheModel
             ngram.JMModel mix.InverseMixModel mix.NestedModel]
            ))

(defn pair->vec [p] [(.left p) (.right p)])

(time-mins (do

;(def train-set (java.io.File. (expand-home "~/opt/src/SLP-Core")))
(def train-set (java.io.File. (expand-home )))

(defn ngram-java
  ([train-path] (ngram-java train-path train-path))
  ([train-path test-path]
   (let [train-set (java.io.File. (expand-home train-path))
         test-set (java.io.File. (expand-home test-path))]

     (doto-class LexerRunner
                 (setLexer (JavaLexer.))
                 (setPerLine false)
                 (addSentenceMarkers true)
                 (useExtension "java"))

     (let [jm-giga-model (JMModel. (GigaCounter.))]

     (doto-class ModelRunner
                 (perLine false)
                 (selfTesting (.equals train-set test-set))
                 (setNGramOrder 6)
                 (learn jm-giga-model train-set))

     (let [nested-model (NestedModel. test-set jm-giga-model)
           mix-model (doto (InverseMixModel. nested-model (CacheModel.))
                           (.setDynamic true))]

       (let [modeled-files (->> (ModelRunner/model mix-model test-set)
                            .iterator iterator-seq (map pair->vec) (into {}))]

       {:model model :modeled-files modeled-files}))))))

(def ngram-res (ngram-java "~/atom-finder/src/java"))

(def statistics (->> ngram-res :modeled-files ModelRunner/getStats))

(printf "Modeled %d tokens, average entropy:\t%.4f\n" (.getCount statistics) (.getAverage statistics))
))

