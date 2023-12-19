(ns d18bis
  (:require
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.test :refer [deftest testing is]]
    [taoensso.encore :as e]
    [util :as u])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private IN
  "d18.in")

(def ^:private t1
"R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)")

(def ^:private DIRS
  {:L [0 -1]
   :R [0 1]
   :U [-1 0]
   :D [1 0]})

(defn parse [s]
  (->> s
    str/split-lines
    (mapv #(let [[_ d n c] (re-matches #"([RDLU])\s+(\d+)\s+\(#(\w+)\)" %)]
             [(keyword (e/have! d)) (parse-long n) c]))))

(defn- trace1 [p [d cnt]]
  (mapv + p (mapv #(* cnt %) (DIRS d))))

(defn- xsect [xs1 xs2]
  (apply +
    (for [[a1 a2] (->> xs1 sort (partition 2))
          [b1 b2] (->> xs2 sort (partition 2))
          :let [x1 (max a1 b1)
                x2 (min a2 b2)]
          :when (>= x2 x1)]
      (- x2 x1 -1))))

(defn- step [[y sq xs] ps]
  (let [y' (ffirst ps)
        xs' (set (map peek ps))
        off (set/intersection xs xs')
        xs'' (set/difference (into xs xs') off)
        width (->> xs sort (partition 2) (map #(dec (apply - %))) (apply +) -)
        xx (xsect xs xs'')]
    [y' (+ sq (- (* width (- y' y -1)) xx)) xs'']))

(defn- solve [moves]
  (->> moves
    (reductions trace1 [0 0])
    (sort-by first)
    (partition-by first)
    (reduce step [0 0 #{}])
    second))

(defn one [s]
  (->> s parse solve))

(deftest t-1
  (is (= (one t1) 62)))

(defn- parse2 [s]
  (for [l (str/split-lines s)
        :let [[_ n d] (re-matches #".*\s\(#(\w{5})(\w)\)" l)]]
    [([:R :D :L :U] (parse-long d)) (Long/parseLong n 16)]))

(defn two [s]
  (-> s parse2 solve))

(deftest t-2
  (is (= (two t1) 952408144115)))

(defn -main [& args]
  (let [input (slurp (or (first args) IN))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
