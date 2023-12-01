(ns d01
  (:require
    [clojure.string :as str]
    [clojure.test :refer :all]
    [taoensso.encore :as e]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private t1
"1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

(defn one [s]
  (->> s
    str/split-lines 
    (map #(let [s (re-seq #"\d" %)] (parse-long (str (first s) (last s)))))
    (apply +)))

(deftest t-1
  (is (= (one t1) 142)))

(def ^:private DIGITS
  {"one" 1 "two" 2 "three" 3 "four" 4 "five" 5 "six" 6 "seven" 7 "eight" 8
   "nine" 9})

(def ^:private RE2-1
  (re-pattern (str "(\\d|" (str/join "|" (keys DIGITS)) ")")))

(def ^:private RE2-2
  (re-pattern (str "(?:.*)" RE2)))

(deftest t-2-re
  (let [s "aaaaninebb11bbbonexx"]
    (is (= (last (re-find RE2-1 s)) "nine"))
    (is (= (last (re-find RE2-2 s)) "one")))

  (let [s "nineight"]
    (is (= (last (re-find RE2-1 s)) "nine"))
    (is (= (last (re-find RE2-2 s)) "eight"))))

(defn- parse-long2 [s]
  (-> s DIGITS (or (parse-long s)) (e/have! :data s)))

(defn two [s]
  (apply +
    (for [l (str/split-lines s)]
      (->> [RE2-1 RE2-2]
        (map #(->> l (re-find %) e/have! last parse-long2))
        (apply str)
        parse-long))))

(def ^:private t2
"two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(deftest t-2
  (is (= (two t2) 281)))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
