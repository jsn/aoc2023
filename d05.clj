(ns d05
  (:require
    [clojure.string :as str]
    [clojure.test :refer :all]
    [clojure.data.avl :as avl]
    [taoensso.encore :as e]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private IN
  (str *ns* ".in"))

(def ^:private t1
"seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

(defn- parse-map [s]
  (let [[head & tail] (str/split-lines s)
        [_ src dst] (re-matches #"(\w+)-to-(\w+)\s.*" head)]
    [(keyword src)
     [(keyword dst)
      (into (avl/sorted-map)
        (for [t tail
              :let [[d s l] (map parse-long (re-seq #"\d+" t))]]
          [s [d l]]))]]))

(defn- parse [s]
  (let [[seeds & maps] (str/split s #"\n\n")]
    {:seeds (mapv parse-long (re-seq #"\d+" seeds))
     :maps (into {} (map parse-map maps))}))

(defn- lookup [mp v]
  (let [[src [dst len]] (avl/nearest mp <= v)]
    (if (and src (< v (+ src len)))
      (+ dst (- v src))
      v)))

(defn one [s]
  (let [{:keys [seeds maps]} (parse s)]
    (loop [stage :seed
           src seeds]
      (let [[dstage mp] (e/have! (maps stage))
            dst (map #(lookup mp %) src)]
        (if (= dstage :location)
          (apply min dst)
          (recur dstage dst))))))

(deftest t-1
  (is (= (one t1) 35)))

(defn- map-range [mp [s0 l]]
  (loop [rv []
         s s0
         l l]
    (let [[src [dst len]] (avl/nearest mp <= s)]
      (if (and src (< s (+ src len)))
        (let [xlen (min l (- (+ src len) s))
              l' (- l xlen)
              s1 (+ s xlen)
              rv' (conj rv [(+ dst (- s src)) xlen])]
          (if (zero? l')
            rv'
            (recur rv' s1 l')))
        (let [[src [dst len]] (avl/nearest mp > s)]
          (if-not src
            (conj rv [s l])
            (let [xlen (min l (- src s))
                  l' (- l xlen)
                  s1 (+ s xlen)
                  rv' (conj rv [s xlen])]
              (if (zero? l')
                rv'
                (recur rv' s1 l')))))))))

(defn two [s]
  (let [{:keys [seeds maps]} (parse s)
        seeds (partition 2 seeds)]
    (loop [stage :seed
           src seeds]
        (let [[dstage mp] (e/have! (maps stage))
              dst (mapcat #(map-range mp %) src)]
          (if (= dstage :location)
            (apply min (map first dst))
            (recur dstage dst))))))

(deftest t-2
  (is (= (two t1) 46)))

(defn -main [& args]
  (let [input (slurp (or (first args) IN))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
