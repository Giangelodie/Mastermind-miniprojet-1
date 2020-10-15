(defproject theme01-clojure "1.0.4"
  :description "Exercices sur le thème 1 : bases de Clojure"
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :profiles {:dev {:dependencies [[midje "1.9.6" :exclusions [org.clojure/clojure]]]
                   :plugins [[lein-midje "3.2.1"]
                             [nightlight/lein-nightlight "2.4.0"]]}
             :midje {}}
  :main Mastermind.mastermind)
