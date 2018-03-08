(defproject
  clojure-training
  "0.1.0"
 
  :source-paths  ["src"]
  :repositories  [["clojars" {:url "https://repo.clojars.org/"}]
                  ["maven-central" {:url "https://repo1.maven.org/maven2"}]]
 
  :dependencies   [[org.clojure/clojure "1.8.0"]
                   [org.clojure/core.async "0.2.374"]
                   [io.pedestal/pedestal.service "0.5.2"]
                   [io.pedestal/pedestal.immutant "0.5.2"]]

  :repl-options {:init-ns clojure-training.core 
                 :port 5555}

  :profiles {:user {:plugins [[cider/cider-nrepl "0.14.0"]]}
                   :dependencies [[org.clojure/tools.nrepl "0.2.10"]]})
            
            
            
                            
                  

