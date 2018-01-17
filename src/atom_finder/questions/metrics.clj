;; What is the distribution of people who commit atoms?

(ns atom-finder.questions.metrics
  (:require
   [atom-finder.util :refer :all]
   [clojure.pprint :refer :all]
   [clojure.string :as string]
   [swiss.arrows :refer :all]
   [schema.core :as s]
   )
  (:import [ch.hsr.ifs.cdt.metriculator.checkers LSLOCScopedASTVisitor])
  )

;; LSLOCMetricChecker - Requires Plugin stuff
;; LSLOCMetric - requires reference to MetricChecker, which needs plugin stuff

(def builder (ch.hsr.ifs.cdt.metriculator.model.HybridTreeBuilder. "atom-finder"))

(def metric-root (->> "int main() {
int x = 3;
if (x) {
return 2;
} else {
return 3;
}
}

int f() {
 return 4;
}
" (mem-tu "file.c")))

(def fileSystemLeaf
  (ch.hsr.ifs.cdt.metriculator.model.AbstractTreeBuilder/createTreeFromPath
   (org.eclipse.core.runtime.Path. "random/file.c")
   metric-root))

(def fileSystemTop (.getRoot fileSystemLeaf))

(def currentScopeNode (.addChild builder (.root builder) fileSystemTop))
(def currentScopeNode (.getChildBy builder (.getHybridId fileSystemLeaf)))

(def lsloc-m (LSLOCScopedASTVisitor. currentScopeNode builder))
(def cc-m (ch.hsr.ifs.cdt.metriculator.checkers.McCabeScopedASTVisitor. currentScopeNode builder))

'((-> metric-root (.accept cc-m)))
