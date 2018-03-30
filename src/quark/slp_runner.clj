;; Use SLP-core to learn an N-gram model
;; Direct port of SLP-Core/src/main/java/slp/core/example/JavaRunner.java

(ns quark.tree-tokenizer
  (:require [atom-finder.util :refer :all]
            [atom-finder.constants :refer :all]
            [schema.core :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str])
  (:import
            [slp.core.counting.giga GigaCounter]
            [slp.core.lexing LexerRunner code.JavaLexer]
            [slp.core.modeling ModelRunner CacheModel
             ngram.JMModel mix.InverseMixModel mix.NestedModel]
            ))

(def train (java.io.File. (expand-home "~/opt/src/SLP-Core")))
(def test train)

(. LexerRunner
  (setLexer (JavaLexer.))
  (setPerLine false)
  (addSentenceMarkers true)
  (useExtension "java"))

(def model (JMModel. (GigaCounter.)))

(. ModelRunner
   (perLine false)
   (selfTesting (train.equals test))
   (setNGramOrder 6)
   (learn model train))

(def model (NestedModel. test model))
(def model (InverseMixModel. model (CacheModel.)))
(.setDynamic model true)

(def modeledFiles (ModelRunner/model model test))
(def statistics (ModelRunner/getStats modeledFiles))

(System.out/printf "Modeled %d tokens, average entropy:\t%.4f\n" (.getCount statistics) (.getAverage statistics))
