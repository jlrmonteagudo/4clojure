(defproject
  clojure-training
  "0.1.0"
 
  :source-paths  ["src"]
  :repositories  [["clojars" {:url "https://repo.clojars.org/"}]
                  ["maven-central" {:url "https://repo1.maven.org/maven2"}]]
 
  :dependencies   [[org.clojure/clojure "1.8.0"]]
                   
  :plugins       [[lein-cljfmt "0.5.7"]]           

  :repl-options {:init-ns clojure-training.core 
                 :port 5555}

  :profiles {:user {:plugins [[cider/cider-nrepl "0.14.0"]]}
                   :dependencies [[org.clojure/tools.nrepl "0.2.10"]]})
            
            
            
                            
                  

