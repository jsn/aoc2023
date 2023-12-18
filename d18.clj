(ns d18
  (:require
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.test :refer [deftest testing is]]
    [taoensso.encore :as e]
    [util :as u])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private IN
  (str *ns* ".in"))

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

(defn- dig1 [{:keys [p] :as g} [d n c]]
  (reduce
    (fn [{:keys [cells]} i]
      (let [v (mapv #(* (inc i) %) (DIRS d))
            p' (mapv + p v)]
      {:cells (assoc cells p' c)
       :p p'}))
    g
    (range n)))

(defn- size [{:keys [cells] :as g}]
  (let [ps (keys cells)
        ys (map first ps)
        xs (map last ps)]
    (assoc g
      :p0 [(apply min ys) (apply min xs)]
      :p1 [(apply max ys) (apply max xs)])))

(defn- neigh [{:keys [p0 p1 cells]} p]
  (assert (not (cells p)))
  (for [d (vals DIRS)
        :let [p' (mapv + p d)]
        :when (and
                (not (cells p'))
                (every? true? (map >= p' p0))
                (every? true? (map <= p' p1)))]
    p'))

(defn- flood [{:keys [p0 p1 cells] :as g} r]
  (loop [border #{r}
         seen #{}]
    (let [seen' (into seen border)
          border' (->> border (mapcat #(neigh g %)) (remove seen) set)]
      (if (seq border')
        (recur border' seen')
        (if (some
              (fn [p] (some true? (concat (map = p0 p) (map = p1 p))))
              seen')
          (set
            (for [y (range (p0 0) (inc (p1 0)))
                  x (range (p0 1) (inc (p1 1)))
                  :when (not (seen' [y x]))]
              [y x]))
          seen')))))

(defn one [s]
  (let [g (->> s
            parse
            (reduce dig1 {:cells {[0 0] "xxx"} :p [0 0]})
            size)
        fl (flood g [1 1])]
    (+ (count (:cells g)) (count fl))))

(deftest t-1
  (is (= (one t1) 62)))

(defn- trace1 [p [d cnt]]
  (mapv + p (mapv #(* cnt %) (DIRS d))))

(defn- parse2 [s]
  (for [l (str/split-lines s)
        :let [[_ n d] (re-matches #".*\s\(#(\w{5})(\w)\)" l)]]
    [([:R :D :L :U] (parse-long d)) (Long/parseLong n 16)]))

(defn- ins [xs]
  (->> xs sort rest (partition 2)))

(defn- xsect [xs1 xs2]
  (apply +
    (for [[a1 a2] (ins xs1)
          [b1 b2] (ins xs2)
          :let [x1 (max a1 b1)
                x2 (min a2 b2)]
          :when (>= x2 x1)]
      (- x2 x1 -1))))

(defn hit2 [y xs vedges]
  (let [vy (ffirst vedges)
        ends (->> vedges (mapcat rest) set)
        xs' (set/difference xs ends)
        xs'' (sort (into xs' (remove xs ends)))
        width (->> xs ins (map #(dec (apply - %))) (apply +) -)
        xx (xsect xs xs'')]
    [(- (* width (- vy y -1)) xx) (set xs'')]))

#_(hit2 0 (set [-10 10]) [[2 1 4]])
#_(hit2 2 #{1 4 -10 10} [[5 2 4]])

(defn two [s]
  (let [vedges
        (->> s
          parse2
          (reductions trace1 [0 0])
          (partition 2 1)
          (keep (fn [[[y1 x1] [y2 x2]]]
                  (when (= y1 y2) (into [y1] (sort [x1 x2])))))
          (sort-by first))
        xmax (apply max (mapcat rest vedges))
        xmin (apply min (mapcat rest vedges))
        ymin (apply min (map first vedges))]
    (loop [vedges vedges
           y (- ymin 10)
           xs (set [(+ -10 xmin) (+ 10 xmax)])
           sq 0]
      (if (empty? vedges) sq
        (let [vy (ffirst vedges)
              [ves vedges'] (split-with #(= (first %) vy) vedges)
              [dsq xs'] (hit2 y xs ves)]
          (recur vedges' vy xs' (+ sq ^long dsq)))))))

(deftest t-2
  (is (= (two t1) 952408144115)))

(defn -main [& args]
  (let [input (slurp (or (first args) IN))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
