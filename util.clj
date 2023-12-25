(ns util
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]
    [clojure.data.priority-map :as prio-map]))

(defn string->vector [s] (read-string (str \[ s \])))

(defn scan-ints [s]
  (mapv parse-long (re-seq #"-?\d+" s)))

(deftest everything
  (testing "string->vector"
    (is (= (string->vector "10,20,30,-1") [10 20 30 -1]))))

(defn astar [graph dist h start goal?]
  (loop [{:keys [queue came-from] :as env}
         {:queue (prio-map/priority-map start (h start))
          :came-from {start [0 nil]}}]
    (let [[cur _] (peek queue)
          [gcur _] (came-from cur)]
      (if (goal? cur)
        (loop [cur cur
               rv '()]
          (let [rv (cons cur rv)
                [_ prev] (came-from cur)]
            (if-not prev
              rv
              (recur prev rv))))
        (when (seq queue)
          (recur
            (reduce
              (fn [env n]
                (let [gn (+ gcur (dist cur n))
                      [gprev _] (came-from n)]
                  (if (and gprev (>= gn gprev))
                    env
                    (-> env
                      (update :queue assoc n (+ gn (h n)))
                      (update :came-from assoc n [gn cur])))))
              (update env :queue pop)
              (graph cur))))))))

(deftest astar-test
  (testing "basic"
    (let [g1 {:a [:b :c :d]
              :c [:e :f]
              :f [:g]}]
      (is (=
           (astar g1 (constantly 1) (constantly 100) :a #{:g})
           [:a :c :f :g])))))

#_(require '[astar.core :as astar])
#_(let [g1 {:a [:b :c :d]
              :c [:e :f]
              :f [:g]}]
      (astar/route g1 (constantly 1) (constantly 100) :a :g))

#_(let [g2 {:a [:b :c]
            :b [:d]
            :c [:d]
            :d [:e]}
        h #(get {:d 1 :e 0 :c 30} % 25)
        dist #(get {[:c :d] 20} [%1 %2] 25)]
    [(astar/route g2 dist h :a :e)
     (astar g2 dist h :a #{:e})])

(defn parse-grid
  ([s] (parse-grid identity s))
  ([f s]
   (let [v (mapv f (str/split-lines s))]
     {:cells v :dims [(count v) (count (first v))]})))

(defn cell [{:keys [cells] :as g} p]
  (get-in cells p))

#_(cell (parse-grid vec
"#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.") [0 1])

(defn gcd [a b]
  (if (zero? b) a (recur b (mod a b))))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(defn xgcd [a b]
  (if (= a 0)
    [b 0 1]
    (let [[g x y] (xgcd (mod b a) a)]
      [g (- y (* (quot b a) x)) x])))

#_(xgcd 120 23)

(defn gen-crt [[o1 p1] [o2 p2]]
  (let [[g u1 u2] (xgcd p1 p2)]
    (when (= (mod o1 g) (mod o2 g))
      (let [p (lcm p1 p2)]
        [(mod (/ (+ (* o1 u2 p2) (* o2 u1 p1)) g) p) p]))))

#_(gen-crt [0 1] [3 4])
#_(gen-crt [1 6] [3 4])

(defn flood [g p]
  (loop [border #{p}
         seen #{}]
    (let [seen' (into seen border)
          border' (->> border (mapcat g) (remove seen') set)]
      (cond->> seen' (seq border') (recur border')))))
