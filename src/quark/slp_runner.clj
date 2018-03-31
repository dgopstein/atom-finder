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

(time-mins
 (do
                                        ;
;(def train (java.io.File. (expand-home "~/atom-finder/src/java")))
(def train-set (java.io.File. (expand-home "~/atom-finder/src/java")))
(def test-set train-set)

(. LexerRunner (setLexer (JavaLexer.)))
(. LexerRunner (setPerLine false))
(. LexerRunner (addSentenceMarkers true))
(. LexerRunner (useExtension "java"))

(def model (JMModel. (GigaCounter.)))

(. ModelRunner (perLine false))
(. ModelRunner (selfTesting (.equals train-set test-set)))
(. ModelRunner (setNGramOrder 6))
(. ModelRunner (learn model train-set))

(def model (NestedModel. test-set model))
(pprn model)
(def model (InverseMixModel. model (CacheModel.)))
(.setDynamic model true)

(def modeledFiles (ModelRunner/model model test-set))

(def mf (->> modeledFiles .iterator iterator-seq (map pair->vec) (into {})))
;(def mf2 mf)

(def statistics (ModelRunner/getStats mf))
(pprn statistics)

(printf "Modeled %d tokens, average entropy:\t%.4f\n" (.getCount statistics) (.getAverage statistics))
)
)

;(slp.core.example.JavaRunner/main (into-array String [(expand-home
;                                                       ;"~/opt/src/SLP-Core"
;                                                       "~/atom-finder/src/java"
;                                                       )]))
