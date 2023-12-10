(ns d10bis
  (:require
    [clojure.string :as str]
    [clojure.test :refer [deftest testing is]]
    [taoensso.encore :as e]
    [util :as u])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private IN
  "d10.in")

(def ^:private t1
"..F7.
.FJ|.
SJ.L7
|F--J
LJ...")

(def ^:private TILES
  {\F #{[1 0] [0 1]}
   \L #{[0 -1] [1 0]}
   \J #{[-1 0] [0 -1]}
   \7 #{[-1 0] [0 1]}
   \- #{[-1 0] [1 0]}
   \| #{[0 -1] [0 1]}})

(defn- parse [s]
  (let [cells (vec (str/split-lines s))
        [start]
        (keep-indexed #(when-let [x (str/index-of %2 \S)] [x %1]) cells)]
    {:w (count (first cells)) :h (count cells) :cells cells :start start}))

(defn- cell [{:keys [cells]} [x y]]
  (get-in cells [y x]))

(defn- flood1 [{:keys [cells] :as world} p]
  (let [c (cell world p)]
    (for [d (if (= c \S) [[-1 0] [1 0] [0 -1] [0 1]] (e/have! (TILES c)))
          :let [p' (mapv + p d)
                c' (cell world p')]
          :when p'
          :let [ds (TILES c')]
          :when (contains? ds (mapv - d))]
      p')))

(defn- solve1 [{:keys [w h cells start] :as world}]
  (loop [seen #{}
         border #{start}
         len 0]
    (let [seen' (into seen border)
          border' (->> border (mapcat #(flood1 world %)) (remove seen') set)]
      (if-not (seq border')
        [len seen']
        (recur seen' border' (inc len))))))

(defn one [s]
  (-> s parse solve1 first))

(deftest t-1
  (is (= (one t1) 8)))

(def ^:private t2
"...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........")

(def ^:private t3
".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...")

(def ^:private t4
"FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L")

(defn- patch-start [{:keys [start cells] :as world}]
  (let [[x y] start
        xs (->> start (flood1 world) (remove #(= start %)) set)

        [tile]
        (for [[tile ds] TILES
              :let [ps (map #(map + start %) ds)]
              :when (every? xs ps)]
          tile)]
    (update-in world [:cells y] #(apply str (-> % vec (assoc x tile))))))

(defn two [s]
  (let [{:keys [path w h] :as world} (patch-start (parse s))
        [_ path] (solve1 world)]
    (->>
      (for [y (range h)]
        (reductions
          (fn [[in edge] x]
            (if (path [x y])
              (case (cell world [x y])
                \- [in edge]
                \| [(not in) \|]
                \F [(not in) \F]
                \L [(not in) \L]
                \J [(cond-> in (= \L edge) not) \J]
                \7 [(cond-> in (= \F edge) not) \7])
              [in nil]))
          [false nil]
          (range w)))
      (apply concat)
      (remove second)
      (filter first)
      count)))

(deftest t-2
  (is (= (two t2) 4))
  (is (= (two t3) 8))
  (is (= (two t4) 10)))

(defn -main [& args]
  (let [input (slurp (or (first args) IN))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
