(ns cbf.core
  (:require [clojure.java.io :as io]
            [esfj.provider :refer [defprovider]]
            [esfj.grapht :refer [bind-provider]])
  (:import [java.util Collection]
           [org.grouplens.lenskit
            ItemRecommender ItemScorer Recommender RecommenderBuildException]
           [org.grouplens.lenskit.core LenskitConfiguration LenskitRecommender]
           [org.grouplens.lenskit.basic AbstractItemScorer]
           [org.grouplens.lenskit.data.dao
            EventDAO ItemDAO UserDAO UserEventDAO]
           [org.grouplens.lenskit.data.event Rating]
           [org.grouplens.lenskit.scored ScoredId]
           [cbf TFIDFModel]
           [cbf.dao
            CSVItemTagDAO ItemTagDAO ItemTitleDAO MOOCItemDAO MOOCRatingDAO
            MOOCUserDAO RatingFile TagFile TitleFile UserFile UserNameDAO]
           [org.grouplens.lenskit.vectors
            MutableSparseVector SparseVector VectorEntry VectorEntry$State]))

(defn build-tag-id-map
  [^ItemTagDAO dao]
  (zipmap (range) (.getTagVocabulary dao)))

(defprovider TFIDFModelProvider TFIDFModel [ItemTagDAO])
(defn tfidf-model
  [^ItemTagDAO dao]
  (let [tag-ids (zipmap (.getTagVocabulary dao) (range))
        doc-freq (doto (-> tag-ids vals MutableSparseVector/create)
                   (.fill 0))
        work (-> tag-ids vals MutableSparseVector/create)
        items (doall
               (map (fn [item]
                      (.clear work)
                      ;; TODO Populate the work vector with the number of times
                      ;;      each tag is applied to this item.
                      ;; TODO Increment the document frequency vector once for
                      ;;      each unique tag on the item.
                      [item (.shrinkDomain work)])
                    (.getItemIds dao)))
        _ (doseq [e (.fast doc-freq)]
            ;; TODO Update this document frequency entry to be a
            ;;      log-IDF value
            )
        model (map (fn [[item tv]]
                     ;; TODO Convert this vector to a TF-IDF vector
                     ;; TODO Normalize the TF-IDF vector to be a unit vector
                     ;; HINT The method tv.norm() will give you the Euclidian
                     ;;      length of the vector
                     [item (.freeze tv)])
                   items)]
    (TFIDFModel. tag-ids (into {} model))))

(defn item-scorer
  [f]
  (reify ItemScorer
    (^void score [_ ^long user ^MutableSparseVector output]
      (f user output))
    (^SparseVector score [this ^long user ^Collection items]
      (let [scores (MutableSparseVector/create items)]
        (.score this user scores)
        (.freeze scores)))
    (^double score [this ^long user ^long item]
      (-> this (.score user [item]) (.get item Double/NaN)))))

(defn make-uvec
  [dao model user]
  (let [ratings (.getEventsForUser dao user Rating)]
    (if (nil? ratings)
      (SparseVector/empty)
      (let [profile (doto (.newTagVector model) (.fill 0))]
        (doseq [r ratings, :let [p (.getPreference r)]
                :when (and p (-> p .getValue (>= 3.5)))]
          ;; TODO Get the item's vector and add it to the user's
          ;; profile
          )
        (.freeze profile)))))

(defprovider TFIItemScorerProvider ItemScorer [UserEventDAO TFIDFModel])
(defn tfidf-item-scorer
  [^UserEventDAO dao ^TFIDFModel model]
  (item-scorer
   (fn [^long user ^MutableSparseVector output]
     (doseq [:let [uvec (make-uvec dao model user)]
             e (.fast output VectorEntry$State/EITHER)
             :let [iv (->> e .getKey (.getItemVector model))]]
       ;; TODO Compute the cosine of this item and the user's profile, store it
       ;;      in the output vector
       ;; TODO And remove this exception to say you've implemented it
       (throw (UnsupportedOperationException. "stub implementation"))))))

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

    ;; use the Clojure TF-IDF model builder
    (bind-provider TFIDFModel TFIDFModelProvider (partial partial tfidf-model))
    ;; use the Clojure TF-IDF scorer to score items
    (bind-provider ItemScorer TFIItemScorerProvider
                   (comp constantly tfidf-item-scorer))))

(defn run
  [users]
  (let [config (configure-recommender)
        rec (LenskitRecommender/build config)
        irec (.getItemRecommender rec)]
    (try
      (doseq [user users, :let [recs (.recommend irec user 5)]]
        (when (empty? recs)
          (println "no recommendations for user:" user "do they exist?"))
        (printf "recommendations for user %d:\n" user)
        (doseq [^ScoredId id recs]
          (printf "  %d: %.4f\n" (.getId id) (.getScore id))))
      (catch UnsupportedOperationException e
        (when (= (.getMessage e) "stub implementation")
          (println "Congratulations, the stub builds and runs!"))))))

(defn -main
  [& args]
  (run (mapv #(Long/parseLong %) args)))
