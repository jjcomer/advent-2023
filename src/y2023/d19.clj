(ns y2023.d19
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

;; PROBLEM LINK https://adventofcode.com/2023/day/19

;; Generator Logic

(defn parse-predicate [pred]
  (let [[p d] (str/split pred #":")]
    (if d
      {:key (keyword (str (first p)))
       :func (case (second p) \< < \> >)
       :com (parse-long (subs p 2))
       :dest d}
      {:dest p})))

(defn parse-workflows [ws]
  (reduce (fn [acc w]
            (let [[w-name preds] (str/split w #"\{|\}")]
              (assoc acc w-name (mapv parse-predicate (str/split preds #",")))))
          {}
          (str/split-lines ws)))

(defn parse-tests [ts]
  (mapv (fn [t]
          (-> t
              (str/replace "{" "{:")
              (str/replace "," " :")
              (str/replace "=" " ")
              (edn/read-string)))
        (str/split-lines ts)))

;; Solution Logic

(defn eval-predicates [predicates test]
  (some (fn [{:keys [key func com dest]}]
          (if key
            (when (func (get test key) com) dest)
            dest))
        predicates))

(defn eval-test [workflows test]
  (loop [state "in"]
    (case state
      "A" (reduce + (vals test))
      "R" 0
      (let [predicates (get workflows state)
            next-state (eval-predicates predicates test)]
        (recur next-state)))))

(defn compute-range-count [ranges]
  (transduce
   (map (fn [[start end]]
          (inc (- end start))))
   *
   (vals ranges)))

(defn get-total-combinations [workflows state ranges]
  (case state
    "R" 0
    "A" (compute-range-count ranges)
    (loop [total 0
           ranges ranges
           preds (get workflows state)]
      (if-let [{:keys [key func com dest]} (first preds)]
        (if key
          (let [[c-start c-end] (get ranges key)]
            (if (< c-start com c-end)
              (let [[new-range remaining-range] (if (= < func)
                                                  [(assoc ranges key [c-start (dec com)])
                                                   (assoc ranges key [com c-end])]
                                                  [(assoc ranges key [(inc com) c-end])
                                                   (assoc ranges key [c-start com])])]
                (recur (+ total (get-total-combinations workflows dest new-range))
                       remaining-range
                       (rest preds)))
              (recur total
                     ranges
                     (rest preds))))
          (recur (+ total (get-total-combinations workflows dest ranges))
                 ranges
                 (rest preds)))
        total))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (let [[workflows tests] (str/split input #"\n\n")]
    [(parse-workflows workflows)
     (parse-tests tests)]))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [[workflows tests]]
  (transduce (map #(eval-test workflows %))
             +
             tests))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [[workflows _]]
  (get-total-combinations workflows "in" {:x [1 4000] :m [1 4000] :a [1 4000] :s [1 4000]}))
