(ns d03
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
"467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(defn- parse [s]
  (let [arr (vec (str/split-lines s))
        h (count arr)
        w (count (first arr))]
    (reduce
      (fn [{:keys [cells labels] :as world} [[x y] c]]
        (if (re-matches #"\d" c)
          (let [prev (when (pos? x) (cells [(dec x) y]))

                [li labels]
                (if (number? prev)
                  [prev (update labels prev str c)]
                  [(count labels) (conj labels c)])]
            (-> world
              (assoc-in [:cells [x y]] li)
              (assoc :labels labels)))
          (assoc-in world [:cells [x y]] c)))
      {:w w
       :h h
       :cells {}
       :labels []}
      (for [y (range h)
            x (range w)
            :let [c (get-in arr [y x])]
            :when (not= c \.)]
        [[x y] (str c)]))))

#_(parse t1)

(defn- neighs [{:keys [h w]} [x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not (= dx dy 0))
        :let [x1 (+ x dx)
              y1 (+ y dy)]
        :when (and (< -1 x1 w) (< -1 y1 h))]
    [x1 y1]))

#_(neighs (parse t1) [9 9])

(defn one [s]
  (let [{:keys [cells labels] :as world} (parse s)]
    (->>
      (for [[p v] cells
            :when (string? v)]
        (->> p (neighs world) (keep cells) (filter number?)))
      (apply concat)
      distinct
      (map (comp parse-long labels))
      (apply +))))

(deftest t-1
  (is (= (one t1) 4361)))

#_(parse (slurp IN))

(defn two [s]
  (let [{:keys [cells labels] :as world} (parse s)]
    (apply +
      (for [[p v] cells
            :when (= "*" v)
            :let [lis
                  (->> p (neighs world) (keep cells) (filter number?) distinct)]
            :when (= 2 (count lis))]
        (apply * (map (comp parse-long labels) lis))))))

(deftest t-2
  (is (= (two t1) 467835)))

(defn -main [& args]
  (let [input (slurp (or (first args) IN))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
