(defproject atom-finder "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 [org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/data.csv "0.1.3"]
                 [org.eclipse.core/org.eclipse.core.resources "3.6.0.v20100526-0737"]
                 [com.ibm.icu/icu4j "58.1"]
                 [clj-jgit "0.8.9"]
                 [org.slf4j/slf4j-nop "1.7.13"] ; suppress version-mismatch warning from jgit https://github.com/sbt/sbt-git/pull/105/files
                 [prismatic/schema "1.1.3"]
                 [com.grammarly/omniconf "0.2.5"] ;https://github.com/grammarly/omniconf
                 [org.clojure/data.csv "0.1.3"]
                 [com.googlecode.java-diff-utils/diffutils "1.3.0"]
                 ]
  :resource-paths ["resources/org.eclipse.cdt.core_6.2.0.201612061315.jar"
                   "resources/com.zutubi.diff-3.1.dev.dgopstein.jar"
                   "resources/APTED_2017-04-05.jar"
                   "resources/changedistiller-0.0.1-SNAPSHOT-jar-with-dependencies.jar"
                   "src/test/resources"
                   "src/conf"]
  :java-source-paths ["src/java"]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]]}}
  :jvm-opts ["-Xss8m" "-Xmx16g"]
  :main atom-finder.core)
