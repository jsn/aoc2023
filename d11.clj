(ns d11
  (:require
    [clojure.string :as str]
    [clojure.math :as math]
    [clojure.test :refer [deftest testing is]]
    [taoensso.encore :as e]
    [util :as u])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private IN
  (str *ns* ".in"))

(def ^:private t1
"...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....")

(defn- parse [s]
  (apply concat
    (map-indexed
      (fn [y l]
        (keep-indexed (fn [x t] (when (= t \#) [x y])) l))
      (str/split-lines s))))

(defn- expand-c [c v d things]
  (mapv #(cond-> % (> (% c) v) (update c + d)) things))

(defn- slice-empty? [c v things]
  (not-any? #(= (% c) v) things))

(defn solve [s d]
  (let [things (parse s)
        ethings
        (reduce
          (fn [things [c v]]
            (cond->> things (slice-empty? c v things) (expand-c c v d)))
          things
          (for [c [0 1]
                v (->> things (map #(% c)) (apply max) range reverse)]
            [c v]))]

    (apply +
      (for [i1 (range 1 (count ethings))
            i2 (range i1)]
        (->> [i1 i2] (map ethings) (apply map (comp abs -)) (apply +))))))

(defn one [s]
  (solve s 1))

(deftest t-1
  (is (= (one t1) 374)))

(deftest t-2
  (is (= (solve t1 9) 1030))
  (is (= (solve t1 99) 8410)))

(defn two [s]
  (solve s (dec 1000000)))

(defn -main [& args]
  (let [input (slurp (or (first args) IN))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
