(defproject atom-finder "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.eclipse.core/org.eclipse.core.resources "3.6.0.v20100526-0737"]
                 [com.ibm.icu/icu4j "58.1"]]
  :resource-paths ["resources/org.eclipse.cdt.core_5.6.0.201402142303.jar"
                   "resources/org.eclipse.equinox.common_3.6.200.v20130402-1505.jar"
                   "src/test/resources"]
  :jvm-opts ["-Xss4m"]
  :main atom-finder.core)
