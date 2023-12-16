(ns d16
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
".|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|....")

(def ^:private MIR
  {\| {[0 -1] [[-1 0] [1 0]]
       [0 1] [[-1 0] [1 0]]}
   \- {[-1 0] [[0 -1] [0 1]]
       [1 0] [[0 -1] [0 1]]}
   \\ {[0 -1] [[1 0]]
       [1 0] [[0 -1]]
       [0 1] [[-1 0]]
       [-1 0] [[0 1]]}
   \/ {[0 -1] [[-1 0]]
       [-1 0] [[0 -1]]
       [0 1] [[1 0]]
       [1 0] [[0 1]]}})

(defn- cell [{:keys [cells]} p]
  (get-in cells p))

(defn- step [{:keys [dims] :as g} [p v]]
  (let [p' (mapv + p v)
        v- (mapv - v)]
    (when-let [c (cell g p')]
      (for [v' (get-in MIR [c v-] [v])]
        [p' v']))))

(defn- solve [g start]
  (print \.)
  (flush)
  (loop [es #{}
         ls #{start}
         seen #{}]
    (let [ls' (distinct (mapcat #(step g %) ls))
          es' (into es (map first ls'))]
      (if (every? seen ls')
        (count es)
        (recur es' ls' (into seen ls'))))))

(defn one [s]
  (solve (u/parse-grid vec s) [[0 -1] [0 1]]))

(deftest t-1
  (is (= (one t1) 46)))

(defn- starts2 [{:keys [dims]}]
  (for [c [0 1]
        :let [C ([1 0] c)]
        [cx cv] [[-1 1] [(dims c) -1]]
        Cx (range (dims C))]
    [(assoc [Cx Cx] c cx) (assoc [0 0] c cv)]))

#_(starts2 (u/parse-grid vec t1))
                    
(defn- two [s]
  (let [g (u/parse-grid vec s)]
    (apply max (pmap #(solve g %) (starts2 g)))))

(deftest t-2
  (is (= (two t1) 51)))

(defn -main [& args]
  (let [input (slurp (or (first args) IN))]
    (println "1." (one input))
    (println "2." (two input))
    (shutdown-agents)))

(comment
  (-main))
