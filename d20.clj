(ns d20
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
"broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a")

(def ^:private t2
"broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output")

(defn- parse [s]
  (into {}
    (for [l (str/split-lines s)
          :let [[_ t k ds] (e/have! (re-matches #"([&%])?(\w+) -> (\w.*)" l))
                ds (str/split ds #",\s+")]]
      [(keyword k) [(mapv keyword ds) t]])))

(defn- initial-state [mods]
  (reduce
    (fn [state [src dst]] (assoc-in state [dst src] false))
    {}
    (for [src (keys mods)
        dst (get-in mods [src 0])
        :when (= "&" (get-in mods [dst 1]))]
    [src dst])))

(defn- run1 [mods state]
  (let [q (conj clojure.lang.PersistentQueue/EMPTY
            [false :broadcaster :button])]
    (loop [q q
           state state
           cnt {false 0 true 0}]
      (if (empty? q)
        [state cnt]
        (let [[v node src] (peek q)
              [ds t] (mods node)
              st (state node)

              [st' rv]
              (case t
                "%" (if v [st nil] [(not st) (not st)])
                "&" (let [st' (assoc st src v)]
                      [st' (not-every? true? (vals st'))])
                nil [nil v])

              state' (assoc state node st')
              q' (cond-> (pop q)
                   (some? rv)
                   (into (for [d ds] [rv d node])))]
          (recur q' state' (update cnt v inc)))))))

(defn one [s]
  (let [mods (parse s)
        state (initial-state mods)]
    (loop [cnt 0
           cnts {}
           state state]
      (if (= cnt 1000)
        (apply * (vals cnts))
        (let [[state dcnt] (run1 mods state)]
          (recur (inc cnt) (merge-with + cnts dcnt) state))))))

(deftest t-1
  (is (= (one t1) 32000000))
  (is (= (one t2) 11687500)))

(defn- trace1 [mods start stop]
  (loop [border #{start}
         seen #{start stop}]
    (let [seen' (into seen border)
          border' (->> border (mapcat #(get-in mods [% 0])) (remove seen') set)]
      (if (empty? border')
        (-> mods
          (select-keys (disj seen' stop))
          (assoc :broadcaster [[start] nil]))
        (recur border' seen')))))

(defn- period [mods]
  (let [state (initial-state mods)]
    (loop [cnt 0
           seen {state 0}
           state state]
      (let [cnt (inc cnt)
            [state _] (run1 mods state)]
        (if-let [prev (seen state)]
          (- cnt prev)
          (recur cnt (assoc seen state cnt) state))))))

(defn two [s]
  (let [mods (parse s)
        state (initial-state mods)
        [qb & more] (for [[k [ds _]] mods :when (some #(= :rx %) ds)] k)]
    (assert (empty? more))
    (reduce u/lcm
      (for [start (get-in mods [:broadcaster 0])]
        (period (trace1 mods start qb))))))

(defn -main [& args]
  (let [input (slurp (or (first args) IN))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
