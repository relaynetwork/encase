(defproject com.relaynetwork/encase "1.0.8"
  :description "Command Pattern Library for Clojure"
  :lein-release { :scm :git }
  :plugins [[lein-release/lein-release "1.0.4"]
            [lein-swank "1.4.5"]]
  :profiles             {:dev {:dependencies [[swank-clojure "1.4.3"]]}
                         ;; NB: the use of ex-info prevents 1.3 from being supported
                         :1.2 {:dependencies [[org.clojure/clojure "1.2.1"]]}
                         :1.3 {:dependencies [[org.clojure/clojure "1.3.0"]]}
                         :1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}
                         :1.5 {:dependencies [[org.clojure/clojure "1.5.1"]]}
                         :1.6 {:dependencies [[org.clojure/clojure "1.6.0-master-SNAPSHOT"]]}}

  :aliases              {"all" ["with-profile" "dev,1.2:dev,1.3:dev,1.4:dev,1.5:dev,1.6"]}
  :dependencies [
                 [org.clojure/data.json "0.2.2"]
                 [com.github.kyleburton/clj-etl-utils "1.0.79"]
  ])
