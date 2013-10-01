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

(defprovider TFIDFModelProvider TFIDFModel [ItemTagDAO])
(defn tfidf-model
  [^ItemTagDAO dao]
  (let [tag-ids (zipmap (.getTagVocabulary dao) (range))
        ^ MutableSparseVector doc-freq
        , (doto (-> tag-ids vals MutableSparseVector/create)
            (.fill 0))
        ^MutableSparseVector work
        , (-> tag-ids vals MutableSparseVector/create)
        items (doall
               (map (fn [item]
                      (.clear work)
                      (->> (.getItemTags dao item)
                           (map tag-ids)
                           (reduce #(assoc! %1 %2 (inc (get %1 %2 0.0)))
                                   (transient {}))
                           (persistent!)
                           (reduce (fn [_ [^long tag ^double n]]
                                     (.add doc-freq tag 1.0)
                                     (.set work tag n))
                                   nil))
                      [item (.shrinkDomain work)])
                    (.getItemIds dao)))
        _ (doseq [:let [ndocs (count items)]
                  ^VectorEntry e (.fast doc-freq)
                  :let [df (.getValue e)] :when (pos? df)]
            (.set doc-freq (.getKey e) (->> df (/ ndocs) Math/log)))
        model (map (fn [[item ^MutableSparseVector tv]]
                     (doseq [^VectorEntry e (.fast tv),
                             :let [tag (.getKey e)
                                   tf (.getValue e)
                                   idf (.get doc-freq tag)]]
                       (.set tv tag (* tf idf)))
                     (.multiply tv (-> tv .norm / double))
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
      (-> (.score this user ^Collection [item])
          (.get item Double/NaN)))))

(defn make-uvec
  [^UserEventDAO dao ^TFIDFModel model user]
  (let [ratings (.getEventsForUser dao user Rating)]
    (if (nil? ratings)
      (SparseVector/empty)
      (let [profile (doto (.newTagVector model) (.fill 0))]
        (doseq [^Rating r ratings, :let [p (.getPreference r)]
                :when (and p (-> p .getValue (>= 3.5)))
                :let [item (.getItemId r)]]
          (.add profile (.getItemVector model item)))
        (.multiply profile (-> profile .norm / double))
        (.freeze profile)))))

(defprovider TFIItemScorerProvider ItemScorer [UserEventDAO TFIDFModel])
(defn tfidf-item-scorer
  [^UserEventDAO dao ^TFIDFModel model]
  (item-scorer
   (fn [^long user ^MutableSparseVector output]
     (doseq [:let [^SparseVector uvec (make-uvec dao model user)]
             ^VectorEntry e (.fast output VectorEntry$State/EITHER)
             :let [tag (.getKey e), iv (.getItemVector model tag)]]
       (.set output tag (.dot uvec iv))))))

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
