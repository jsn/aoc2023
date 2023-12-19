(ns d19
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
"px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}")

(defn- parse-rules [s]
  (reduce
    (fn [a l]
      (let [[_ k rules] (e/have! (re-matches #"(\w+)\{(.+)\}" l))
            rules
            (mapv
              #(let [[_ p op v dst]
                     (e/have! (re-matches #"(?:(\w)([<=>])(\d+):)?(\w+)" %))]
                 (into
                   [(keyword dst)]
                   (when p
                     [(keyword p) (keyword op) (parse-long v)])))
              (str/split rules #","))]
        (assoc a (keyword k) rules)))
    {}
    (str/split-lines s)))

(defn- parse-parts [s]
  (mapv
    (fn [l]
      (into {}
        (for [c (str/split (subs l 1 (dec (count l))) #",")
              :let [[k v] (str/split c #"=")]]
          [(keyword k) (parse-long v)])))
    (str/split-lines s)))

(def ^:private OPS
  {:> > :< < := =})

(defn- run-rule [rule part]
  (let [[d k op v] rule]
    (cond
      (not k) d
      ((e/have! (OPS op)) (k part) v) d
      :else nil)))

(defn- run-flows [flows part]
  (loop [flow :in]
    (let [rules (e/have! (flows flow))
          d (e/have! (some #(run-rule % part) rules))]
      (case d
        (:A :R) d
        (recur d)))))

(defn one [s]
  (let [[rules parts] (str/split s #"\n\n")
        flows (parse-rules rules)
        parts (parse-parts parts)]
    (apply + (mapcat #(when (= :A (run-flows flows %)) (vals %)) parts))))

(deftest t-1
  (is (= (one t1) 19114)))

(defn- op [op k v part]
  (let [[v0 v1] (k part)]
    (case op
      :< (cond
           (< v1 v) [part]
           (or (= v 1) (>= v0 v)) [nil part]
           :else [(assoc-in part [k 1] (dec v)) (assoc-in part [k 0] v)])
      :> (cond
           (> v0 v) [part]
           (or (= v 4000) (<= v1 v)) [nil part]
           :else [(assoc-in part [k 0] (inc v)) (assoc-in part [k 1] v)])
      := (if-not (<= v0 v v1)
           [nil part]
           [(into [(assoc part k [v v])]
              (remove nil?
                [(when (< v0 v) (assoc-in part [k 1] (dec v)))
                 (when (< v v1) (assoc-in part [k 0] (inc v)))]))]))))

(defn- score [part]
  (apply *
    (for [[a b] (vals part)]
      (- b a -1))))

(defn two [s]
  (let [[rules parts] (str/split s #"\n\n")
        flows (parse-rules rules)
        parts (parse-parts parts)
        init {:x [1 4000] :m [1 4000] :a [1 4000] :s [1 4000]}]
    (loop [q [[init :in 0]]
           ac {:A [] :R []}]
      (if-let [[part flow pos] (peek q)]
        (let [[d k o v] (e/have! (get-in flows [flow pos]))
              [a & rems] (if (not k) [part] (op o k v part))
              q' (into (pop q) (for [r rems] [r flow (inc pos)]))]
          (if (ac d)
            (recur q' (update ac d conj a))
            (recur (conj q' [a d 0]) ac)))
        (apply + (map score (:A ac)))))))

(deftest t-2
  (is (= (two t1) 167409079868000)))

(defn -main [& args]
  (let [input (slurp (or (first args) IN))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
