(ns y2023.d9
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2023/day/9

;; Generator Logic

(defn parse-line [line]
  (mapv parse-long (str/split line #" ")))

;; Solution Logic

(defn zeros? [nums]
  (every? zero? nums))

(defn gen-differences [nums]
  (loop [nums [nums]]
    (if (zeros? (last nums))
      nums
      (let [diffs (mapv (fn [[a b]] (- b a)) (partition 2 1 (last nums)))]
        (recur (conj nums diffs))))))

(defn find-diff [lookup op nums]
  (let [diffs (gen-differences nums)]
    (reduce (fn [last-diff diff] (op diff last-diff))
            0
            (map lookup (reverse diffs)))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input
       str/split-lines
       (mapv parse-line)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (transduce (map #(find-diff last + %)) + input))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (transduce (map #(find-diff first - %)) + input))
