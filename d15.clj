(ns d15
  (:require
    [clojure.string :as str]
    [clojure.test :refer [deftest testing is]]
    [taoensso.encore :as e]
    [flatland.ordered.map :as omap]
    [util :as u])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private IN
  (str *ns* ".in"))

(def ^:private t1
"rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(defn- cksum [s]
  (reduce #(-> %2 long (+ %1) (* 17) (rem 256)) 0 s))

(defn- parse [s]
  (-> s str/split-lines first (str/split #",")))

(defn one [s]
  (->> s parse (map cksum) (apply +)))

(deftest t-1
  (is (= (one t1) 1320)))

(defn two [s]
  (let
    [boxes
     (reduce
       (fn [boxes ins]
         (let [[_ lbl op arg] (e/have! (re-matches #"(\w+)([-=])(\d+)?" ins))
               box (cksum lbl)]
           (case op
             "-" (update boxes box dissoc lbl)
             "=" (update boxes box assoc lbl (e/have! (parse-long arg))))))
       (->> (omap/ordered-map) (repeat 256) vec)
       (parse s))]
    (apply +
      (for [i (range 256)
            li*n (map-indexed #(* (inc %1) (val %2)) (boxes i))]
        (* (inc i) li*n)))))

(deftest t-2
  (is (= (two t1) 145)))

(defn -main [& args]
  (let [input (slurp (or (first args) IN))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
