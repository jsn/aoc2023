(ns d08
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
"RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)")

(defn- parse-graph [s]
  (into {}
    (->> s
      str/split-lines
      (map #(->> %
              (re-seq #"\w+")
              ((juxt first (comp vec rest))))))))

(defn- parse [s]
  (let [[steps graph] (str/split s #"\n\n")]
    [steps (parse-graph graph)]))

(defn one [s]
  (let [[steps graph] (parse s)]
    (->> steps
      cycle
      (reductions
        (fn [node step]
          (-> node graph (get ({\L 0 \R 1} step))))
        "AAA")
      (reduce #(cond-> (inc %1) (= "ZZZ" %2) reduced) -1))))

(def ^:private t2
"LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)")

(deftest t-1
  (is (= (one t1) 2)
  (is (= (one t2) 6))))

(def ^:private t3
"LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")

(defn- trace1 [steps graph start]
  (loop [rv [start]
         n 0
         node start
         seen {[0 start] 0}]
    (let [i (rem n (count steps))
          n (inc n)
          step ({\L 0 \R 1} (get steps i))
          node (get-in graph [node step])
          k [i node]]
      (if-let [prev-n (seen k)]
        (let [period (- n prev-n)
              zs (keep-indexed #(when (str/ends-with? %2 "Z") %1) rv)]
          (e/have! (set zs) period))
        (recur (conj rv node) n node (assoc seen k n))))))

(defn gcd [a b]
  (if (zero? b) a (recur b (mod a b))))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(defn two [s]
  (let [[steps graph] (parse s)]
    (->> graph
      keys
      (filter #(str/ends-with? % "A"))
      (map #(trace1 steps graph %))
      (reduce lcm))))

(deftest t-2
  (is (= (two t3) 6)))

(defn -main [& args]
  (let [input (slurp (or (first args) IN))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
