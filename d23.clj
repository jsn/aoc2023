(ns d23
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
"#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#")

(defn- parse [s]
  (let [{:keys [dims cells] :as g} (u/parse-grid vec s)
        [sx] (keep-indexed #(when (= \. %2) %1) (first (:cells g)))
        [dx] (keep-indexed #(when (= \. %2) %1) (peek (:cells g)))]
    (assoc g :src [0 sx] :dst [(dec (dims 0)) dx])))

(def ^:private SLOPES
  {\> [0 1]
   \< [0 -1]
   \^ [-1 0]
   \v [1 0]})

(defn- neigh [{:keys [cells] :as g} p]
  (for [d (if-let [d (SLOPES (get-in cells p))]
            [d]
            [[-1 0] [1 0] [0 -1] [0 1]])
        :let [p' (mapv + p d)
              c (get-in cells p')]
        :when (and c (not= \# c))]
    p'))

(defn- advance [g [p seen]]
  (for [p' (neigh g p)
        :when (not (seen p'))]
    [p' (conj seen p')]))

(defn one [s]
  (let [{:keys [src] :as g} (parse s)]
    (loop [border [[src #{src}]]]
      (let [border' (mapcat #(advance g %) border)]
        (if (empty? border')
          (->> border first peek count dec)
          (recur border'))))))

(deftest t-1
  (is (= (one t1) 94)))

(defn- neigh2 [{:keys [cells] :as g} p]
  (for [d [[-1 0] [1 0] [0 -1] [0 1]]
        :let [p' (mapv + p d)
              c (get-in cells p')]
        :when (and c (not= \# c))]
    p'))

(defn- trace1 [g s p]
  (loop [s s
         p p
         cnt 1]
    (let [ps (remove #(= % s) (neigh2 g p))]
      (if (= 1 (count ps))
        (recur p (first ps) (inc cnt))
        [p cnt]))))

(defn- compress [{:keys [dims] :as g}]
  (let [nodes
        (for [y (range (dims 0))
              x (range (dims 1))
              :when (not= (get-in g [:cells y x]) \#)
              :when (not= 2 (count (neigh2 g [y x])))]
          [y x])]
    (reduce
      (fn [graph p]
        (assoc graph p
          (into {}
            (for [p' (neigh2 g p)]
              (trace1 g p p')))))
      {}
      nodes)))

(defn- graph [gr [p seen]]
  (for [p' (keys (gr p))
        :when (not (seen p'))]
    [p' (conj seen p')]))

(defn- dist [gr [p _] [q _]]
  (- (get-in gr [p q])))

(defn two [s]
  (let [{:keys [src dst] :as g} (parse s)
        gr (compress g)
        graph #(graph gr %)
        dist #(dist gr %1 %2)
        h (fn [[p seen]] (if (= p dst) 0 (- (get-in gr [dst p] 1000000000))))
        path (u/astar graph dist h [src #{src}] #(= (first %) dst))]
    (->> path (map first) (partition 2 1) (map #(get-in gr %)) (apply +))))

(deftest t-2
  (is (= (two t1) 154)))

(defn -main [& args]
  (let [input (slurp (or (first args) IN))]
    (time (println "1." (one input)))
    (time (println "2." (two input)))))

(comment
  (-main))
