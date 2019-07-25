(defproject morelinear "0.1.0-SNAPSHOT"
  :description "linea-systems in Clojure"
  :url "http://geokon-gh.github.io/morelinear/index.html"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 [org.clojure/clojure "1.10.0"]
                 [net.mikera/core.matrix "0.62.0"]]
  :main ^:skip-aot morelinear.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
