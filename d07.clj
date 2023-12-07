(ns d07
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
"32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483")

(defn- htype [s]
  (let [freqs (->> s frequencies vals (remove #{1}) sort reverse)]
    (condp = freqs
      [5] 10
      [4] 9
      [3 2] 8
      [3] 7
      [2 2] 6
      [2] 5
      [] 4
      (throw (ex-info "bad htype" {:freqs freqs})))))

(def ^:private CARDS "AKQJT98765432")

(defn- make-cmp [htype cards]
  (fn [a b]
    (let [rv (compare (htype a) (htype b))]
      (if-not (zero? rv)
        rv
        (first
          (remove zero?
            (map
              (fn [& args]
                (->> args
                  (map #(- (str/index-of cards %)))
                  (apply compare)))
              a b)))))))

(defn one [s]
  (let [cards
        (into {}
          (for [l (str/split-lines s)
                :let [[c v] (str/split l #"\s+")]]
            [c (parse-long v)]))]
    (->> cards
      keys
      (sort (make-cmp htype CARDS))
      (map cards)
      (map * (iterate inc 1))
      (apply +))))

(deftest t-1
  (is (= (one t1) 6440)))

(def ^:private CARDS2 "AKQT98765432J")

(defn- htype2 [s]
  (let [freqs (->> s frequencies)
        jf (get freqs \J 0)

        freqs
        (if (= jf 5)
          [5]
          (-> freqs (dissoc \J) vals sort reverse vec (update 0 + jf)))]
    (condp = (remove #{1} freqs)
      [5] 10
      [4] 9
      [3 2] 8
      [3] 7
      [2 2] 6
      [2] 5
      [] 4
      (throw (ex-info "bad htype" {:freqs freqs})))))

(defn two [s]
  (let [cards
        (into {}
          (for [l (str/split-lines s)
                :let [[c v] (str/split l #"\s+")]]
            [c (parse-long v)]))]
    (->> cards
      keys
      (sort (make-cmp htype2 CARDS2))
      (map cards)
      (map * (iterate inc 1))
      (apply +))))

(deftest t-2
  (is (= (two t1) 5905)))

(defn -main [& args]
  (let [input (slurp (or (first args) IN))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
