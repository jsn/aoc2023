(ns d06bis
  (:require
    [clojure.string :as str]
    [clojure.test :refer :all]
    [taoensso.encore :as e]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private IN "d06.in")

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

(defn- bsearch [a b f]
  (let [fa (f a)
        fb (f b)]
    (e/have! true? (not= fa fb))
    (e/have! true? (< a b))
    (loop [a a b b]
      (if (= b (inc a))
        a
        (let [p (quot (+ a b) 2)
              fp (f p)]
          (if (= fp fa)
            (recur p b)
            (recur a p)))))))

(defn two [s]
  (let [[t0 d0]
        (map #(->> % (re-seq #"\d+") str/join parse-long) (str/split-lines s))
        m (quot t0 2)
        f #(> (* (- t0 %) %) d0)]
    (- (bsearch m t0 f) (bsearch 0 m f))))

(deftest t-2
  (is (= (two t1) 71503)))

(defn -main [& args]
  (let [input (slurp (or (first args) IN))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
