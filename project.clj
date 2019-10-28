(defproject travelling-salesman-problem "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [uncomplicate/neanderthal "0.26.1"]
                 [criterium "0.4.4"]]
  :plugins [[lein-marginalia "0.9.1"]]
  :main ^:skip-aot travelling-salesman-problem.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
