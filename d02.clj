(ns d02
  (:require
    [clojure.string :as str]
    [clojure.test :refer :all]
    [taoensso.encore :as e]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private t1
"Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(defn- parse-game [s]
  (->> s
    (re-seq #"(\d+)\s+(\w+)")
    (map (fn [[_ cnt k]] [(keyword k) (parse-long cnt)]))
    (into {})))

(defn- parse-line [l]
  (let [[_ id more] (e/have! (re-matches #"Game\s+(\d+):\s*(.*)" l))]
    [(parse-long id) (mapv parse-game (str/split more #";\s*"))]))

(defn- parse [s]
  (->> s str/split-lines (map parse-line)))

(def ^:private COND1
  {:red 12 :green 13 :blue 14})

(defn- good1? [g]
  (every? (fn [[k v]] (>= (COND1 k) v)) g))

(defn one [s]
  (->> s
    parse
    (keep (fn [[id games]] (when (every? good1? games) id)))
    (apply +)))

(deftest t-1
  (is (= (one t1) 8)))

(defn two [s]
  (->> s parse
    (map
      (fn [[id games]]
        (apply * (map #(apply max 0 (keep % games)) [:green :red :blue]))))
    (apply +)))

(deftest t-2
  (is (= (two t1) 2286)))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
