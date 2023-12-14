(ns d14
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
"O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....")

(defn- cell [{:keys [cells]} p]
  (get-in cells p))

(defn- fall1 [{:keys [dims] :as g} [y0 x]]
  (if (= \O (cell g [y0 x]))
    (loop [y y0]
      (let [y' (dec y)]
        (if (or (neg? y') (not= (cell g [y' x]) \.))
          (-> g
            (assoc-in [:cells y0 x] \.)
            (assoc-in [:cells y x] \O))
          (recur y'))))
    g))

(defn- score [{:keys [dims] :as g}]
  (apply +
    (for [y (range (dims 0))
          x (range (dims 1))
          :when (= \O (cell g [y x]))]
      (- (dims 0) y))))

(defn one [s]
  (let [{:keys [dims] :as g} (u/parse-grid vec s)]
    (score
      (reduce
        (fn [g y]
          (reduce #(fall1 %1 [y %2]) g (range (dims 1))))
        g
        (range 1 (dims 0))))))

(deftest t-1
  (is (= (one t1) 136)))

(defn- fall2 [{:keys [dims cells] :as g} [c d] p]
  (if (= \O (cell g p))
    (loop [v (p c)]
      (let [v' (+ v d)
            p' (assoc p c v')]
        (if (or (neg? v') (= (dims c) v') (not= (cell g p') \.))
          (assoc g :cells
            (-> cells
              (assoc-in p \.)
              (assoc-in (assoc p c v) \O)))
          (recur v'))))
    g))

(defn- cycle1 [{:keys [dims] :as g}]
  (reduce #(apply fall2 %1 %2) g
    (for [[c d :as dir] [[0 -1] [1 -1] [0 1] [1 1]]
          vc (cond-> (range (dims c)) (pos? d) reverse)
          vC (range (dims ([1 0] c)))]
      [dir (assoc [vC vC] c vc)])))

(defn two [s]
  (loop [g (u/parse-grid vec s)
         cnt 0
         seen {g 0}]
    (let [g' (cycle1 g)
          cnt' (inc cnt)]
      (if-let [prev (seen g')]
        (let [period (- cnt' prev)
              r (rem (- 1000000000 prev) period)]
          (score ((e/invert-map seen) (+ r prev))))
        (recur g' cnt' (assoc seen g' cnt'))))))

(deftest t-2
  (is (= (two t1) 64)))

(defn -main [& args]
  (let [input (slurp (or (first args) IN))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
