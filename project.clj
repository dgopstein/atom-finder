(defproject atom-finder "1.0.0"
  :description "Find atoms of confusion in source code"
  :url "atomsofconfusion.com"
  :license {:name "The MIT License (MIT)"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [
                 [org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/data.csv "0.1.3"]
                 [org.eclipse.core/org.eclipse.core.resources "3.6.0.v20100526-0737"]
                 [com.ibm.icu/icu4j "58.1"]
                 [clj-jgit "0.8.10"]
                 [org.slf4j/slf4j-nop "1.7.13"] ; suppress version-mismatch warning from jgit https://github.com/sbt/sbt-git/pull/105/files
                 [prismatic/schema "1.1.3"]
                 [com.grammarly/omniconf "0.2.5"] ;https://github.com/grammarly/omniconf
                 [com.climate/claypoole "1.1.4"]
                 [org.clojure/data.csv "0.1.3"]
                 [com.googlecode.java-diff-utils/diffutils "1.3.0"]
                 [com.google.guava/guava "23.0"]
                 [clojail "1.0.6"]
                 [swiss-arrows "1.0.0"]
                 ;[clj-cdt/clj-cdt "0.0.1"]

                 ;;;;;;;;;;;;;;;;;;;;;;   occasional dependencies   ;;;;;;;;;;;;;;;;;;;;;;
                 ;[org.clojure/math.combinatorics "0.1.4"] ; For calculating plot-ordering p-values in atom-finder paper
                 ;[org.eclipse.platform/org.eclipse.core.resources "3.12.0"] ; Required for include-file resolution - Depends on multiple versions of eclipse libraries and generates warnings... this is not an interaction with another atom-finder dependendency, the problem is upstream
                 ]
  :resource-paths ["resources/org.eclipse.cdt.core_6.2.0.201612061315.jar"
                   "resources/com.zutubi.diff-3.1.dev.dgopstein.jar"
                   ;"resources/changedistiller-0.0.1-SNAPSHOT-jar-with-dependencies.jar"
                   "resources/clj-cdt-0.0.2.jar"
                   "src/test/resources"
                   "src/conf"
                   ]
  :java-source-paths ["src/java"]
  :profiles {:dev             {:dependencies [[org.clojure/test.check "0.9.0"]]}
             :slp             {:dependencies [[org.eclipse.jdt/org.eclipse.jdt.core "3.13.100"
                                               :exclusions [org.eclipse.platform/org.eclipse.core.runtime]]
                                              [org.jboss.marshalling/jboss-marshalling-river "1.4.11.Final"]
                                              [it.unimi.dsi/fastutil "7.2.0"]]
                               :resource-paths ["resources/uber-SLP_Core-0.2.jar"]
                               :java-source-paths ["/Users/dgopstein/opt/src/SLP-Core"]
                               }
             :instrumentation {:main atom-finder.instrumentation
                               :uberjar-name "instrumentation.jar"
                               :aot [atom-finder.instrumentation]
                               :manifest {"Premain-Class" "atom_finder.instrumentation"
                                          "Agent-Class"   "atom_finder.instrumentation"}
                               }
             :find-atoms-in-dirs {:main atom-finder.find-atoms-in-dirs} ; https://stackoverflow.com/questions/11023762/leiningen-with-multiple-main-classes
             }
  :aliases {"find-atoms-in-dirs" ["with-profile" "find-atoms-in-dirs" "run"]}
  :jvm-opts ["-Xss8m" "-Xmx8g" "-XX:+HeapDumpOnOutOfMemoryError"]
  :main atom-finder.core
  ;:aot [atom-finder.core]
  :plugins [[lein-codox "0.10.3"]
            ;[lein-localrepo "0.5.4"] ; needed until clj-cdt is published on clojars
            ]
  ;:codox {:namespaces [#"^atom-finder\.(?!instrumentation|results-sandbox)"
  ;                     #"^atom-finder\.tree-diff\.(?!apted|change-distiller)"]}
)
