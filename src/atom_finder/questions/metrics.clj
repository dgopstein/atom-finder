;; What is the distribution of people who commit atoms?

(ns atom-finder.questions.metrics
  (:require
   [atom-finder.util :refer :all]
   [clojure.pprint :refer :all]
   [clojure.string :as string]
   [swiss.arrows :refer :all]
   [schema.core :as s]
   )
  (:import [ch.hsr.ifs.cdt.metriculator.checkers
            LSLOCMetric LSLOCMetricChecker LSLOCScopedASTVisitor
            McCabeMetric McCabeMetricChecker McCabeScopedASTVisitor]
           [ch.hsr.ifs.cdt.metriculator.model AbstractTreeBuilder]
           [org.eclipse.core.runtime Path]
           [org.eclipse.cdt.core.dom.ast IASTTranslationUnit IASTNode]
           )
  )

;; LSLOCMetricChecker - Requires Plugin stuff
;; LSLOCMetric - requires reference to MetricChecker, which needs plugin stuff

(def builder (ch.hsr.ifs.cdt.metriculator.model.HybridTreeBuilder. "atom-finder"))

(def metric-root (->> "int main() {
int x = 3;
if (x) {
 if (x == 3) {
return 2;
} else {
return 4;
}
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

(s/defn scope-node
  [node]
  (let [fileSystemLeaf (AbstractTreeBuilder/createTreeFromPath (Path. "random/file.c") (root-ancestor node))]
    (.addChild builder (.root builder) (.getRoot fileSystemLeaf))
    (.getChildBy builder (.getHybridId fileSystemLeaf))))

(s/defn mccabe [node :- IASTNode]
  (.aggregate (McCabeMetric. (atom_finder.McCabeMetricCheckerPluginless.) "McCabe" "CC")
              (scope-node node)))

(s/defn lsloc [node :- IASTNode]
  (.aggregate (LSLOCMetric. (atom_finder.LSLOCMetricCheckerPluginless.) "LSLOC" "lines of code")
              (scope-node node)))

'((->> metric-root
     flatten-tree
     (map lsloc)
     prn))
