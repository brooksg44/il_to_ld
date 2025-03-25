(defproject il-to-ld "0.1.0-SNAPSHOT"
  :description "Compiler from IEC 61131-3 IL to LD and vice versa"
  :url "http://example.com/il-to-ld"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [instaparse "1.5.0"]
                 [org.clojure/spec.alpha "0.3.218"]]
  :main ^:skip-aot il-to-ld.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})