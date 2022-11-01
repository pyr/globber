(defproject spootnik/globber "0.4.2"
  :description "globber: globbing searches in clojure"
  :url "https://github.com/pyr/globber"
  :license {:name "MIT License"}
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :deploy-repositories [["releases" :clojars] ["snapshots" :clojars]]
  :profiles {:dev  {:plugins      [[lein-ancient "0.7.0"]]
                    :global-vars  {*warn-on-reflection* true}}
             :test {:plugins      [[lein-difftest "2.0.0"]
                                   [lein-cljfmt "0.9.0"]]
                    :global-vars  {*warn-on-reflection* true}
                    :pedantic?    :abort}})
