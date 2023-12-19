(ns y2023.d19
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

;; PROBLEM LINK https://adventofcode.com/2023/day/19

;; Generator Logic

(defn parse-workflows [ws]
  (reduce (fn [acc w]
            (let [[w-name preds] (str/split w #"\{|\}")]
              (assoc acc w-name (str/split preds #","))))
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
  (some (fn [pred]
          (let [[p dest] (str/split pred #":")]
            (if dest
              (let [k (keyword (str (first p)))
                    f (case (second p) \< < \> >)
                    c (parse-long (subs p 2))]
                (when (f (get test k) c) dest))
              p)))
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
      (if-let [pred (first preds)]
        (let [[p dest] (str/split pred #":")]
          (if dest
            (let [f (second p)
                  c-val (parse-long (subs p 2))
                  c-key (keyword (str (first p)))
                  [c-start c-end] (get ranges c-key)]
              (if (< c-start c-val c-end)
                (let [[new-range remaining-range] (if (= \< f)
                                                    [(assoc ranges c-key [c-start (dec c-val)])
                                                     (assoc ranges c-key [c-val c-end])]
                                                    [(assoc ranges c-key [(inc c-val) c-end])
                                                     (assoc ranges c-key [c-start c-val])])]
                  (recur (+ total (get-total-combinations workflows dest new-range))
                         remaining-range
                         (rest preds)))
                (recur total
                       ranges
                       (rest preds))))
            (recur (+ total (get-total-combinations workflows p ranges))
                   ranges
                   (rest preds))))
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
