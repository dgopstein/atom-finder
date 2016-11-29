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
                 ;[org.wickedsource/diffparser "1.2"]
                 ]
  :resource-paths ["resources/org.eclipse.cdt.core_5.6.0.201402142303.jar"
                   "resources/org.eclipse.equinox.common_3.6.200.v20130402-1505.jar"
                   "resources/com.zutubi.diff-3.0.dev.jar"
                   "src/test/resources"]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]]}}
  :jvm-opts ["-Xss8m"]
  :main atom-finder.core)
