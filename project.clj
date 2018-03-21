(defproject atom-finder "1.0.0"
  :description "Find atoms of confusion in source code"
  :url "atomsofconfusion.com"
  :license {:name "The MIT License (MIT)"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [
                 [org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/data.csv "0.1.3"]
                 [org.clojure/math.combinatorics "0.1.4"] ; just for funzies, can be removed
                 [org.eclipse.core/org.eclipse.core.resources "3.6.0.v20100526-0737"]
                 ;[org.eclipse.platform/org.eclipse.core.resources "3.12.0"] ; Required for include-file resolution - Depends on multiple versions of eclipse libraries and generates warnings... this is not an interaction with another atom-finder dependendency, the problem is upstream
                 [com.ibm.icu/icu4j "58.1"]
                 [clj-jgit "0.8.10"]
                 [org.slf4j/slf4j-nop "1.7.13"] ; suppress version-mismatch warning from jgit https://github.com/sbt/sbt-git/pull/105/files
                 [prismatic/schema "1.1.3"]
                 [com.grammarly/omniconf "0.2.5"] ;https://github.com/grammarly/omniconf
                 [org.clojure/data.csv "0.1.3"]
                 [com.googlecode.java-diff-utils/diffutils "1.3.0"]
                 [com.google.guava/guava "23.0"]
                 [clojail "1.0.6"]
                 [swiss-arrows "1.0.0"]
                 ]
  :resource-paths ["resources/org.eclipse.cdt.core_6.2.0.201612061315.jar"
                   "resources/com.zutubi.diff-3.1.dev.dgopstein.jar"
                   ;"resources/changedistiller-0.0.1-SNAPSHOT-jar-with-dependencies.jar"
                   "src/test/resources"
                   "src/conf"
                   ]
  :java-source-paths ["src/java"]
  :profiles {:dev             {:dependencies [[org.clojure/test.check "0.9.0"]]}
             :instrumentation {:main atom-finder.instrumentation
                               :uberjar-name "instrumentation.jar"
                               :aot [atom-finder.instrumentation]
                               :manifest {"Premain-Class" "atom_finder.instrumentation"
                                          "Agent-Class"   "atom_finder.instrumentation"}
                               }}
  :jvm-opts ["-Xss8m" "-Xmx32g" "-XX:+HeapDumpOnOutOfMemoryError"]
  :main atom-finder.core
  ;:aot [atom-finder.core]
  :plugins [[lein-codox "0.10.3"]]
  ;:codox {:namespaces [#"^atom-finder\.(?!instrumentation|results-sandbox)"
  ;                     #"^atom-finder\.tree-diff\.(?!apted|change-distiller)"]}
)
