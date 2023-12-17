(ns d17
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
"2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533")

(def ^:private TURNS
  {:L {[0 1] [-1 0]
       [-1 0] [0 -1]
       [0 -1] [1 0]
       [1 0] [0 1]}
   :R {[0 1] [1 0]
       [1 0] [0 -1]
       [0 -1] [-1 0]
       [-1 0] [0 1]}})

(defn- graph [{:keys [dims] :as g} [p v cnt]]
  (let [moves (cond-> [:L :R] (< cnt 3) (conj :S))]
    (for [m moves
          :let [v' (if (= :S m) v (e/have! (get-in TURNS [m v])))
                p' (mapv + p v')
                cnt' (if (= :S m) (inc cnt) 1)]
          :when (every? #(< -1 (p' %) (dims %)) [0 1])]
      [p' v' cnt'])))

(defn- h [goalp [p v cnt]]
  (->> p (map - goalp) (map abs) (apply +)))

(defn one [s]
  (let [{:keys [dims cells] :as g}
        (u/parse-grid #(mapv (comp parse-long str) %) s)

        graph #(graph g %)
        dist #(get-in cells (first %2))
        goalp (mapv dec dims)
        h #(h goalp %)
        goal? #(zero? (h %))
        start [[0 0] [0 1] 0]
        path (u/astar graph dist h start goal?)]
    (-
     (apply + (map #(->> % first (get-in cells)) path))
     (get-in cells [0 0]))))

(deftest t-1
  (is (= (one t1) 102)))

(defn- graph2 [{:keys [dims] :as g} [p v cnt]]
  (let [moves (cond
                (zero? cnt) [:L :R :S]
                (< cnt 4) [:S]
                (< cnt 10) [:L :R :S]
                :else [:L :R])]
    (for [m moves
          :let [v' (if (= :S m) v (e/have! (get-in TURNS [m v])))
                p' (mapv + p v')
                cnt' (if (= :S m) (inc cnt) 1)]
          :when (every? #(< -1 (p' %) (dims %)) [0 1])]
      [p' v' cnt'])))

(defn two [s]
  (let [{:keys [dims cells] :as g}
        (u/parse-grid #(mapv (comp parse-long str) %) s)

        graph #(graph2 g %)
        dist #(get-in cells (first %2))
        goalp (mapv dec dims)
        h #(h goalp %)
        goal? #(and (zero? (h %)) (>= (peek %) 4))
        start [[0 0] [0 1] 0]
        path (u/astar graph dist h start goal?)]
    (-
     (apply + (map #(->> % first (get-in cells)) path))
     (get-in cells [0 0]))))

(deftest t-2
  (is (= (two t1) 94)))

(defn -main [& args]
  (let [input (slurp (or (first args) IN))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
