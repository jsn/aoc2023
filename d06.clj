(ns d06
  (:require
    [clojure.string :as str]
    [clojure.test :refer :all]
    [taoensso.encore :as e]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private IN
  (str *ns* ".in"))

(def ^:private t1
"Time:      7  15   30
Distance:  9  40  200")

(defn- n-ways [[t d]]
  (->> t (range 1) (filter #(> (* (- t %) %) d)) count))

(defn one [s]
  (let [[ts ds] (map u/scan-ints (str/split-lines s))
        races (mapv vector ts ds)]
    (apply * (map n-ways races))))

(deftest t-1
  (is (= (one t1) 288)))

(defn two [s]
  (let [[t0 d0]
        (map #(->> % (re-seq #"\d+") str/join parse-long) (str/split-lines s))
        ts (vec (range (inc t0)))
        tl (subvec ts 0 (/ t0 2))
        tr (subvec ts (/ t0 2))
        index #(abs (java.util.Collections/binarySearch %1 d0 %2))
        cmp #(compare (* (- t0 %1) %1) %2)]
    (+ (- (count tl) (index tl cmp)) (index tr (comp - cmp)))))

(deftest t-2
  (is (= (two t1) 71503)))

(defn -main [& args]
  (let [input (slurp (or (first args) IN))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
