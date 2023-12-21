(ns d21
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
"...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........")

(defn parse [s]
  (let [{:keys [dims cells] :as g} (u/parse-grid vec s)
        [start]
        (for [y (range (dims 0))
              x (range (dims 1))
              :when (= \S (get-in cells [y x]))]
          [y x])]
    g (assoc g :s start)))

(defn- neigh [{:keys [cells]} p]
  (for [d [[-1 0] [1 0] [0 -1] [0 1]]
        :let [p' (mapv + p d)]
        :when (#{\S \.} (get-in cells p'))]
    p'))

(defn- flood-n [g n]
  (loop [n n
         border #{(:s g)}]
    (let [n (dec n)
          border' (->> border (mapcat #(neigh g %)) set)]
      (if (or (empty? border') (zero? n))
        border'
        (recur n border')))))

(defn one [s]
  (-> s parse (flood-n 64) count))

(deftest t-1
  (is (= (-> t1 parse (flood-n 6) count) 16)))

(defn- flood [g start]
  (loop [rv [1]
         border #{start}]
    (let [border' (->> border (mapcat #(neigh g %)) set)
          cnt (count border')
          l (count rv)]
      (if (and (> l 1) (= (rv (- l 2)) cnt))
        rv
        (recur (conj rv cnt) border')))))

(def ^:private traces
  (memoize
    (fn [{:keys [dims] :as g} pc]
      (let [mdims (mapv dec dims)
            p (mapv * pc (mapv #(/ (mdims %) 2) [0 1]))]
        (flood g p)))))

(defn- count-reached ^long [g p cnt]
  (let [cnts (e/have! (traces g p))
        l (count cnts)]
    (if (< cnt l)
      (cnts cnt)
      (cnts (- l 1 (mod (- cnt l -1) 2))))))

(defn- count-line [{:keys [dims] :as g} p tcnt]
  (let [dim (dims (if (even? (first p)) 1 0))
        ncnts (count (traces g p))
        base (- tcnt 1 (quot dim 2))]
    (if (neg? base) 0
      (loop [reached (inc (quot base dim))
             over (mod base dim)
             rv 0]
        (if (zero? reached) rv
          (if (or (< over ncnts) (odd? reached))
            (recur (dec reached) (+ over dim) (+ rv (count-reached g p over)))
            (+ rv
              (* (/ reached 2)
                (+ (count-reached g p over)
                  (count-reached g p (+ over dim)))))))))))

(defn- rsq [r]
  (/ (* (+ r (mod r 2)) (inc (quot r 2))) 2))

(defn- count-triangle [{:keys [dims] :as g} p tcnt]
  (let [dsum (quot (apply + dims) 2)
        ncnts (count (traces g p))
        base (- tcnt 1 dsum)]
    (if (neg? base) 0
      (loop [^long r (inc (quot base dsum))
             over (mod base dsum)
             rv 0]
        (if (zero? r) rv
          (if (or (< over ncnts) (odd? r))
            (recur (dec r) (+ over dsum)
              (+ rv (* r (count-reached g p over))))
            (+ rv
              (* (rsq r) (count-reached g p over))
              (* (rsq (dec r)) (count-reached g p (+ over dsum))))))))))

(defn- solve [g tcnt]
  (into
   [(count-reached g [1 1] tcnt)]
   (concat
     (for [p [[0 1] [1 0] [2 1] [1 2]]]
       (count-line g p tcnt))
     (for [p [[0 0] [0 2] [2 0] [2 2]]]
       (count-triangle g p tcnt)))))

(def ^:private TARGET 26501365)

(defn two [s]
  (apply + (solve (parse s) TARGET)))

(defn -main [& args]
  (let [input (slurp (or (first args) IN))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
