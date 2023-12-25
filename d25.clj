(ns d25
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
"jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr")

(defn- parse [s]
  (reduce
    (fn [a [p q]]
      (-> a
        (update p (fnil conj #{}) q)
        (update q (fnil conj #{}) p)))
    {}
    (for [l (str/split-lines s)
          :let [[p & qs] (map keyword (re-seq #"\w+" l))]
          q qs]
      [p q])))

(defn- try1 [g]
  (let [kv (vec (keys g))
        [n1 n2] (->> #(rand-int (count kv))
                  repeatedly
                  (partition 2 1)
                  (some #(when (apply not= %) %))
                  (map kv))]
    (map #(vec (sort %))
     (partition 2 1
      (u/astar g (constantly 1) (constantly 1) n1 #(= % n2))))))

(defn- flood [g p]
  (loop [border #{p}
         seen #{}]
    (let [seen' (into seen border)
          border' (->> border (mapcat g) (remove seen') set)]
      (if (empty? border')
        (count seen')
        (recur border' seen')))))

(defn one [s]
  (let [g (parse s)
        all (flood g (first (keys g)))]
    (loop [fs {}]
      (let [fs
            (->> #(try1 g)
              (repeatedly 32)
              (apply concat)
              frequencies
              (merge-with + fs))

            edges (->> fs keys (sort-by (comp - fs)) (take 3))

            g'
            (reduce
              (fn [g [p q]]
                (-> g
                  (update p disj q)
                  (update q disj p)))
              g
              edges)

            sizes (map #(flood g' %) (first edges))]
        (if (= (first sizes) all)
          (recur fs)
          (apply * sizes))))))

(deftest t-1
  (is (= (one t1) 54)))

(defn two [s]
  "not implemented")

(deftest t-2
  (is (= (two t1) "not implemented")))

(defn -main [& args]
  (let [input (slurp (or (first args) IN))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
