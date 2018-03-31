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
(def train-set (java.io.File. (expand-home "~/atom-finder/src/java")))
(def test-set train-set)

(doto-class LexerRunner
            (setLexer (JavaLexer.))
            (setPerLine false)
            (addSentenceMarkers true)
            (useExtension "java"))

(def model (JMModel. (GigaCounter.)))

(doto-class ModelRunner
            (perLine false)
            (selfTesting (.equals train-set test-set))
            (setNGramOrder 6)
            (learn model train-set))

(def model (NestedModel. test-set model))
(def model (InverseMixModel. model (CacheModel.)))
(.setDynamic model true)

(def modeledFiles (->> (ModelRunner/model model test-set)
                       .iterator iterator-seq (map pair->vec) (into {})))

(def statistics (ModelRunner/getStats modeledFiles))

(printf "Modeled %d tokens, average entropy:\t%.4f\n" (.getCount statistics) (.getAverage statistics))
))

