(ns cbf.core
  (:require [clojure.java.io :as io])
  (:import [org.grouplens.lenskit
            ItemRecommender ItemScorer Recommender RecommenderBuildException]
           [org.grouplens.lenskit.core
            LenskitConfiguration LenskitRecommender]
           [org.grouplens.lenskit.data.dao
            EventDAO ItemDAO UserDAO]
           [org.grouplens.lenskit.scored ScoredId]
           [cbf
            TFIDFItemScorer TFIDFModel TFIDFModelBuilder]
           [cbf.dao
            CSVItemTagDAO ItemTagDAO ItemTitleDAO MOOCItemDAO MOOCRatingDAO
            MOOCUserDAO RatingFile TagFile TitleFile UserFile UserNameDAO]))

(defn ^:private configure-recommender
  []
  (doto (LenskitConfiguration.)
    ;; configure the rating data source
    (-> (.bind EventDAO) (.to MOOCRatingDAO))
    (-> (.set RatingFile) (.to (io/file "data/ratings.csv")))
    ;; use custom item and user DAOs
    ;; specify item DAO implementation with tags
    (-> (.bind ItemDAO) (.to CSVItemTagDAO))
    ;; specify tag file
    (-> (.set TagFile) (.to (io/file "data/movie-tags.csv")))
    ;; and title file
    (-> (.set TitleFile) (.to (io/file "data/movie-titles.csv")))
    ;; our user DAO can look up by user name
    (-> (.bind UserDAO) (.to MOOCUserDAO))
    (-> (.set UserFile) (.to (io/file "data/users.csv")))
    ;; use the TF-IDF scorer you will implement to score items
    (-> (.bind ItemScorer) (.to TFIDFItemScorer))))

(defn run
  [users]
  (let [config (configure-recommender)
        rec (LenskitRecommender/build config)
        irec (.getItemRecommender rec)]
    (try
      (doseq [user users, :let [recs (.recommend irec user 5)]]
        (when (empty? recs)
          (println "no recommendations for user:" user "do they exist?"))
        (println "recommendations for user:" user)
        (doseq [id recs]
          (->> id .getId (format "\t%d\n") print)))
      (catch UnsupportedOperationException e
        (when (= (.getMessage e) "stub implementation")
          (println "Congratulations, the stub builds and runs!"))))))

(defn -main
  [& args]
  (run (mapv #(Long/parseLong %)
             args)))


