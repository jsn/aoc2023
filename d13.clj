(ns d13
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
"#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#")

(defn- parse1 [s]
  (let [v (vec (str/split-lines s))]
    {:cells v :dims [(count v) (count (first v))]}))

(defn- slice [{:keys [cells dims]} [c v]]
  (->> c [1 0] dims range (map #(get-in cells (assoc [% %] c v)))))

(defn- check-axis [{:keys [cells dims] :as mp} [c v]]
  (let [vmax (dec (dims c))]
    (loop [v v
           v2 (inc v)]
      (when (= (slice mp [c v]) (slice mp [c v2]))
        (or (zero? v) (= v2 vmax) (recur (dec v) (inc v2)))))))

(defn- solve1 [{:keys [dims] :as mp}]
  (first
    (for [c [0 1]
          v (range (dec (dims c)))
          :when (check-axis mp [c v])]
      (* ([100 1] c) (inc v)))))

(defn one [s]
  (->> (str/split s #"\n\n") (map parse1) (map solve1) (apply +)))

(deftest t-1
  (is (= (one t1) 405)))

(defn- check-axis2 [{:keys [cells dims] :as mp} [c v]]
  (let [vmax (dec (dims c))]
    (loop [v v
           v2 (inc v)
           diffs 0]
      (let [cmps (frequencies (map = (slice mp [c v]) (slice mp [c v2])))
            diffs (+ diffs ^long (cmps false 0))]
        (when (<= diffs 1)
          (if (or (zero? v) (= v2 vmax))
            (= diffs 1)
            (recur (dec v) (inc v2) diffs)))))))

(defn- solve2 [{:keys [dims] :as mp}]
  (first
    (for [c [0 1]
          v (range (dec (dims c)))
          :when (check-axis2 mp [c v])]
      (* ([100 1] c) (inc v)))))

(defn two [s]
  (->> (str/split s #"\n\n") (map parse1) (map solve2) (apply +)))

(deftest t-2
  (is (= (two t1) 400)))

(defn -main [& args]
  (let [input (slurp (or (first args) IN))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
