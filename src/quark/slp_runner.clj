;; Use SLP-core to learn an N-gram model
;; Direct port of SLP-Core/src/main/java/slp/core/example/JavaRunner.java

(ns quark.slp-runner
  (:require [schema.core :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [atom-finder.util :refer :all]
            [swiss.arrows :refer :all]
            )
  (:import  [java.io File]
            [slp.core.counting.giga GigaCounter]
            [slp.core.lexing runners.LexerRunner code.JavaLexer]
            [slp.core.modeling runners.ModelRunner CacheModel ngram.JMModel
             mix.MixModel mix.InverseMixModel dynamic.NestedModel]
            [slp.core.translating Vocabulary]
            ))

(defn pair->vec [p] [(.left p) (.right p)])
(def pairlist->seq (%->> .iterator iterator-seq (map pair->vec)))
(def pairlist->map (%->> pairlist->seq (into {})))

(defn init-java-lexer []
  (doto (LexerRunner. (JavaLexer.) false)
    (.setSentenceMarkers true)
    (.setExtension "c")))

(defn init-modeler [train-path test-path & {:keys [self-test] :or {self-test false}}]
  (let [lexer-runner (init-java-lexer)
        vocabulary (Vocabulary.)]
    (-> (JMModel. 6 (GigaCounter.))
         (NestedModel. lexer-runner vocabulary (File. test-path))
         (MixModel/standard (CacheModel.))
         (doto (.setDynamic true))
         (ModelRunner. lexer-runner vocabulary)
         (doto (.learnDirectory (File. train-path))))))

;(def dir-path (expand-home "~/atom-finder/src/java"))
(def dir-path (expand-home "~/opt/src/vim/src/libvterm"))
;(def dir-path (expand-home "~/opt/src/atom-finder"))
;(def dir-path (expand-home "~/opt/src/atom-finder/gecko-dev/testing"))

;; 5 mins on all of atom-finder corpus if modeled results are not calculated/returned
(def ngram-res (time-mins (init-modeler dir-path dir-path :self-test true)))

;(def statistics (-> ngram-res :model-runner (.getStats (-> ngram-res :modeled-files))))

;(printf "Modeled %d tokens, average entropy:\t%.4f\n" (.getCount statistics) (.getAverage statistics))

(defn least-common-lines [model-runner dir-path]
   (let []
     (->> (.modelDirectory model-runner (File. dir-path))
          pairlist->map
          (mapcat (fn [[file problist]]
                    (map-indexed (fn [idx probs] [(str/replace (.getPath file) dir-path "") (inc idx) probs])
                                 problist)))
          ;(min-n-by 200 (%->> last (apply *))) ;; most probable lines
          (min-n-by 50 (%->> last (map #(/ 1 %)) (apply *))) ;; least probable lines
          (remove nil?)
          )))

;; Print the least predictable lines
'((->> (least-common-lines ngram-res dir-path)
     (map (fn [[file line]]
            (print-line-context 0 (str dir-path "/" file) line :top-border false)
            (println "\n")))
     time-mins
     ))

(defn model-all-lines [model-runner dir-path]
    (->> dir-path File.
         (.modelDirectory model-runner)
         pairlist->seq
         (mapcat (fn [[file problist]]
                   (map-indexed (fn [idx probs]
                                  {:file (-> file .getPath (str/replace dir-path ""))
                                   :line (inc idx)
                                   :probability probs})
                                problist)))
         ))

(defmacro ignore-output
  [& block]
  `(with-open [w# (clojure.java.io/writer "/dev/null")]
    (binding [*out* w#, *err* w#]
      ~@block)))

;(def local-dir "gecko-dev")
;(def local-dir "gecko-dev/testing")
;(def local-dir "web-platform/tests/resources/webidl2/test/widlproc/src")
;(def local-dir "web-platform/tests/resources") ; no error
(def local-dir "") ; yes error

(prn (str (now)))
(-<>> local-dir
      (model-all-lines ngram-res)
      (maps-to-csv "atom-finder-token-probabilities-test.csv" {:separator \|}))

     '((map (fn [[file line]]
            (print-line-context 0 (str dir-path "/" file) line :top-border false)
            (println "\n")))
     time-mins
     )
