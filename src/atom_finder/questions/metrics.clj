;; What is the distribution of people who commit atoms?

(ns atom-finder.questions.metrics
  (:require
   [atom-finder.util :refer :all]
   [clojure.pprint :refer :all]
   [clojure.string :as string]
   [swiss.arrows :refer :all]
   [schema.core :as s]
   )
  (:import [ch.hsr.ifs.cdt.metriculator.checkers LSLOCScopedASTVisitor McCabeMetric McCabeMetricChecker McCabeScopedASTVisitor])
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

(def atom-comments-c
  (parse-file (expand-home "~/nyu/confusion/atom-finder/src/test/resources/atom-comments.c")))

(def fileSystemLeaf
  (ch.hsr.ifs.cdt.metriculator.model.AbstractTreeBuilder/createTreeFromPath
   (org.eclipse.core.runtime.Path. "random/file.c")
   metric-root))

(def fileSystemTop (.getRoot fileSystemLeaf))

(def currentScopeNode (.addChild builder (.root builder) fileSystemTop))
(def currentScopeNode (.getChildBy builder (.getHybridId fileSystemLeaf)))


;(def lsloc-m (LSLOCScopedASTVisitor. currentScopeNode builder))
(def cc-m (McCabeScopedASTVisitor. currentScopeNode builder))

'((-> metric-root (.accept cc-m)))

(def mmcp (atom_finder.McCabeMetricCheckerPluginless.))

(def mccabe-key (ch.hsr.ifs.cdt.metriculator.model.AbstractMetric/getKeyFor McCabeMetric))

(.processAst mmcp metric-root)

(.getNodeValue currentScopeNode mccabe-key)

;; Gives an ANSWER!
(.aggregate (McCabeMetric. mmcp "a" "b") currentScopeNode)
