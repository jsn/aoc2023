(ns d04
  (:require
    [clojure.string :as str]
    [clojure.test :refer :all]
    [taoensso.encore :as e]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private IN
  (str *ns* ".in"))

(def ^:private t1
"Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(defn- parse1 [l]
  (let [[_ id xs ys] (re-matches #"Card\s+(\d+):\s+([^|]+)\|\s+(\S.*)" l)]
    (into [(parse-long id)]
      (map #(re-seq #"\d+" %) [xs ys]))))

(defn one [s]
  (->> s
    str/split-lines
    (map parse1)
    (map
      (fn [[_ xs ys]]
        (count (keep (set xs) ys))))
    (filter pos?)
    (map #(long (Math/pow 2 (dec %))))
    (apply +)))

(deftest t-1
  (is (= (one t1) 13)))

(defn two [s]
  (let [cards
        (->> s
          str/split-lines
          (map parse1)
          (map
            (fn [[id xs ys]]
              [id (count (keep (set xs) ys))])))]
    (loop [[[id cnt] & cs] cards
           wins (zipmap (map first cards) (repeat 1))]
      (if-not id
        (apply + (vals wins))
        (let [dwins (into {}
                      (for [d (range cnt)]
                        [(+ id 1 d) (wins id)]))]
          (recur cs (merge-with + wins dwins)))))))

(deftest t-2
  (is (= (two t1) 30)))

(defn -main [& args]
  (let [input (slurp (or (first args) IN))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
