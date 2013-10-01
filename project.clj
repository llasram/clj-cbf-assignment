(defproject clj-cbf-assignment "0.1.0-SNAPSHOT"
  :description "Clojure version of"
  :url "https://gist.github.com/llasram/6472144"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths ["src/clojure"]
  :java-source-paths ["src/java"]
  :main ^:skip-aot cbf.core
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.grouplens.lenskit/lenskit-knn "2.0.2"]
                 [org.grouplens.lenskit/lenskit-core "2.0.2"]
                 [org.grouplens.lenskit/lenskit-api "2.0.2"]
                 [org.grouplens.lenskit/lenskit-data-structures "2.0.2"]
                 [org.platypope/esfj "0.1.1"]
                 [org.slf4j/slf4j-api "1.7.5"]
                 [org.slf4j/slf4j-log4j12 "1.7.5"]
                 [log4j "1.2.17"]])
