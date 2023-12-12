(ns d12
  (:require
    [clojure.string :as str]
    [clojure.test :refer [deftest testing is]]
    [clojure.math.combinatorics :as c]
    [taoensso.encore :as e]
    [util :as u])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private IN
  (str *ns* ".in"))

(def ^:private t1
"???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1")

(defn- hash-counts [v]
  (->> v (partition-by #{\#}) (keep #(when (= (first %) \#) (count %)))))

(defn- calc-counts [[sv cnts]]
  (let [hashes (->> sv hash-counts (apply +))
        hidden (- (apply + cnts) hashes)
        qs (keep-indexed #(when (= %2 \?) %1) sv)
        hdots (- (count qs) hidden)
        reps (concat (repeat hdots \.) (repeat hidden \#))]
    (count
      (for [rep (c/permutations reps)
            :let [tv (reduce #(apply assoc %1 %2) sv (zipmap qs rep))]
            :when (= (hash-counts tv) cnts)]
        tv))))
          
(defn one [s]
  (->> s
    str/split-lines
    (map #(let [[s n-s] (str/split % #"\s")]
            [(vec s) (u/string->vector n-s)]))
    (map calc-counts) (apply +)))

(deftest t-1
  (is (= (one t1) 21)))

(defn- match-cnt [v cnt nh nd]
  (loop [i 0
         nh nh
         ac 0]
    (if (= i (count v))
      (when (= ac cnt) [[] nh nd])
      (case (v i)
        \. (when (= ac cnt) [(subvec v (inc i)) nh nd])
        \# (when-not (= ac cnt) (recur (inc i) nh (inc ac)))
        \? (if (= ac cnt) 
             (when (pos? nd) [(subvec v (inc i)) nh (dec nd)])
             (when (pos? nh) (recur (inc i) (dec nh) (inc ac))))))))

#_(match-cnt [\# \# \#] 3 0 0)
#_(match-cnt [\# \# \?] 3 0 0)
#_(match-cnt [\# \# \?] 3 1 0)
#_(match-cnt [\# \# \.] 2 0 0)
#_(match-cnt [\# \# \?] 2 0 1)

(declare solve1)

(defn- solve1* [v [cnt & more :as cnts] nh nd]
  (case (get v 0)
    nil 1
    \# (let [[v' nh' nd'] (match-cnt v cnt nh nd)]
         (if v'
           (if (= 0 nh' nd')
             1
             (solve1 v' more nh' nd'))
           0))
    \. (recur (subvec v 1) cnts nh nd)
    \?
    (+
     (if (pos? nh)
       (solve1 (assoc v 0 \#) cnts (dec nh) nd)
       0)
     (if (pos? nd)
       (solve1 (assoc v 0 \.) cnts nh (dec nd))
       0))))

(def ^:private solve1 (memoize solve1*))

(defn- calc-counts2 [[sv cnts]]
  (let [{hashes \# qs \? :or {hashes 0 qs 0}} (frequencies sv)
        hidden (- (apply + cnts) hashes)
        hdots (- qs hidden)]
    (solve1 sv cnts hidden hdots)))

#_(calc-counts2 [[\? \? \? \. \# \# \. \. \#] [1 2 1]])
#_(calc-counts2 [[\. \# \# \# \? \? \? \?] [3 1]])
#_(solve1 [\? \? \? \. \# \# \#] [1 1 3] 2 1)
#_(solve1 [\? \? \? \? \.] [1 1] 2 2)
#_(calc-counts [[\? \? \? \? \.] [1 1]])

(defn two [s]
  (->> s
    str/split-lines
    (map #(let [[s n-s] (str/split % #"\s")]
            [(->> s (repeat 5) (str/join "?") vec)
             (->> n-s u/string->vector (repeat 5) (apply concat) vec)]))
    (map calc-counts2)
    (apply +)))

(deftest t-2
  (is (= (two t1) 525152)))

(defn -main [& args]
  (let [input (slurp (or (first args) IN))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
