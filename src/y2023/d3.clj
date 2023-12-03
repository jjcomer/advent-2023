(ns y2023.d3
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2023/day/3

;; Generator Logic

(defn find-line-numbers [line]
  (loop [matcher (re-matcher #"(\d+)" line) numbers {}]
    (if (re-find matcher)
      (let [start (.start matcher)
            end (.end matcher)
            num (parse-long (.group matcher))]
        (recur matcher (assoc numbers (set (range start end)) num)))
      numbers)))

(defn find-numbers [lines]
  (reduce
   (fn [acc [row line]]
     (assoc acc row (find-line-numbers line)))
   {}
   (map-indexed vector lines)))

(defn find-symbols [lines]
  (reduce
   (fn [acc [row line]]
     (loop [acc acc matcher (re-matcher #"([^.|\w])" line)]
       (if (re-find matcher)
         (recur (assoc acc [row (.start matcher)] (.group matcher)) matcher)
         acc)))
   {}
   (map-indexed vector lines)))

;; Solution Logic

(defn generate-neighbours [[row col]]
  (for [x [-1 0 1]
        y [-1 0 1]
        :when (not= x y 0)]
    [(+ row x) (+ col y)]))

(defn find-neighbouring-numbers [numbers coord]
  (vals (reduce
         (fn [acc [row col]]
           (let [matching (reduce-kv (fn [acc cols num]
                                       (if (cols col)
                                         (assoc acc [row cols] num)
                                         acc))
                                     {}
                                     (get numbers row {}))]
             (merge acc matching)))
         {}
         (generate-neighbours coord))))

(defn find-parts [symbols numbers]
  (reduce-kv
   (fn [acc coord _]
     (let [nums (find-neighbouring-numbers numbers coord)]
       (concat acc nums)))
   []
   symbols))

(defn find-gears [symbols numbers]
  (reduce-kv
   (fn [acc coord symbol]
     (if (= "*" symbol)
       (let [nums (find-neighbouring-numbers numbers coord)]
         (if (= 2 (count nums))
           (conj acc (reduce * nums))
           acc))
       acc))
   []
   symbols))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (let [lines (-> input str/trim str/split-lines)]
    [(find-numbers lines) (find-symbols lines)]))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [[numbers symbols]]
  (reduce + (find-parts symbols numbers)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [[numbers symbols]]
  (reduce + (find-gears symbols numbers)))
