(ns d09
  (:require
    [clojure.string :as str]
    [clojure.test :refer [deftest testing is]]
    [taoensso.encore :as e]
    [util :as u])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private IN
  (str *ns* ".in"))

(def ^:private t1
"0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45")

(defn- solve1 [sq]
  (->> sq
    (iterate #(mapv - (rest %) %))
    (take-while #(not-every? zero? %))
    (map peek)
    (apply +)))

(defn one [s]
  (->> s str/split-lines (map (comp solve1 u/string->vector)) (apply +)))

(deftest t-1
  (is (= (one t1) 114)))

(defn- solve2 [sq]
  (->> sq
    (iterate #(mapv - (rest %) %))
    (take-while #(not-every? zero? %))
    (map first)
    reverse
    (reduce (comp - -))))

(defn two [s]
  (->> s str/split-lines (map (comp solve2 u/string->vector)) (apply +)))

(deftest t-2
  (is (= (two t1) 2)))

(defn -main [& args]
  (let [input (slurp (or (first args) IN))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
