(ns d22
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
"1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9")

(defn- parse [s]
  (mapv #(mapv vec (partition 3 (u/scan-ints %))) (str/split-lines s)))

(defn- minz [ps]
  (apply min (map peek ps)))

(defn- b->ps [[p0 p1 :as b]]
  (let [c (or (first (for [c (range 3) :when (not= (p0 c) (p1 c))] c)) 0)
        vs (map #(% c) b)]
    (for [v (range (apply min vs) (inc (apply max vs)))]
      (assoc p0 c v))))

(defn- make-glass [bs]
  (reduce
    (fn [glass i]
      (let [b (bs i)
            ps (set (b->ps b))]
        (-> glass
          (update :ps into (for [p ps] [p i]))
          (assoc-in [:bs i] b))))
    {:ps {} :bs {}}
    (range (count bs))))

(defn- fall1 [glass i]
  (let [b (get-in glass [:bs i])
        mz (minz b)
        ps (b->ps b)
        points' (apply dissoc (:ps glass) ps)]
    (loop [dz 0
           ps ps]
      (let [ps' (mapv #(update % 2 dec) ps)]
        (if (or (= mz (inc dz)) (some points' ps'))
          (-> glass
            (assoc :ps (into points' (for [p ps] [p i])))
            (assoc-in [:bs i] (mapv #(update % 2 - dz) b)))
          (recur (inc dz) ps'))))))

(defn- fall-all [glass]
  ;(print ".")
  ;(flush)
  (->> glass
    :bs
    keys
    (sort-by #(minz (get-in glass [:bs %])))
    (reduce fall1 glass)))

(defn- remove-b [glass i]
  (let [ps (b->ps (get-in glass [:bs i]))]
    (-> glass
      (update :bs dissoc i)
      (update :ps #(apply dissoc % ps)))))

(defn one [s]
  (let [glass (-> s parse make-glass fall-all)
        is (keys (:bs glass))]
    ((frequencies
       (for [i is]
         (let [g (remove-b glass i)]
           (every? true?
             (for [i (keys (:bs g))]
               (= (get-in g [:bs i]) (get-in (fall1 g i) [:bs i])))))))
      true)))

(deftest t-1
  (is (= (one t1) 5)))

(defn two [s]
  (let [glass (-> s parse make-glass fall-all)
        is (keys (:bs glass))]
    (apply +
      (for [i is
            :let [g (remove-b glass i)
                  g' (fall-all g)]]
        (count (filter #(not= (get-in g [:bs %]) (get-in g' [:bs %])) is))))))

(deftest t-2
  (is (= (two t1) 7)))

(defn -main [& args]
  (let [input (slurp (or (first args) IN))]
    (println "1." (time (one input)))
    (println "2." (time (two input)))))

(comment
  (-main))
