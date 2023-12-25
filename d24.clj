(ns d24
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
"19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3")

(defn- parse [s]
  (mapv #(mapv vec (partition 3 (u/scan-ints %))) (str/split-lines s)))

(defn- ->abc [[[x y _] [dx dy _]]]
  [(/ 1 dx) (- (/ 1 dy)) (- (- (/ x dx) (/ y dy)))])

(defn- xsect [v0 v1]
  (let [[a1 b1 c1] (->abc v0)
        [a2 b2 c2] (->abc v1)
        d (- (* a1 b2) (* a2 b1))]
    (when-not (zero? d)
      [(/ (- (* b1 c2) (* b2 c1)) d) (/ (- (* c1 a2) (* c2 a1)) d)])))

(defn- solve [vs minc maxc]
  (apply +
    (for [i0 (range 1 (count vs))
          i1 (range i0)
          :let [[[vx0 vy0 _] [vdx0 vdy0 _] :as v0] (vs i0)
                [[vx1 vy1 _] [vdx1 vdy1 _] :as v1] (vs i1)
                [x y] (xsect v0 v1)]
          :when x
          :when (>= (/ (- x vx0) vdx0) 0)
          :when (>= (/ (- x vx1) vdx1) 0)
          :when (and (<= minc x maxc) (<= minc y maxc))]
      1)))

(defn one [s]
  (-> s parse (solve 200000000000000 400000000000000)))

(deftest t-1
  (is (= (-> t1 parse (solve 7 27)) 2)))

(defn- xop [c v vx]
  (let [[x0 dx] (map #(get-in v [% c]) [0 1])
        dx' (abs (- dx vx))]
    (if (zero? dx') [x0 0] [(mod x0 dx') dx'])))

(defn- try-v [c vs vx]
  (reduce
    (fn [[o p] [o2 p2]]
      (if (zero? p2)
        (if (= o (mod o2 p)) [o p] (reduced nil))
        (or (u/gen-crt [o p] [o2 p2]) (reduced nil))))
    [0 1]
    (map #(xop c % vx) vs)))

(defn- find-c [vs c]
  (loop [vx 0N]
    (when-not (> vx 100000)
      (e/cond
        :if-let [r (try-v c vs vx)] (first r)
        :if-let [r (try-v c vs (- vx))] (first r)
        (recur (inc vx))))))

(defn two [s]
  (let [vs (parse s)]
    (long (apply + (map #(find-c vs %) (range 3))))))

(defn -main [& args]
  (let [input (slurp (or (first args) IN))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
