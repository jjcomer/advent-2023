(ns y2023.d13
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2023/day/13

;; Solution Logic

(defn count-differences [ma mb]
  (count (filter false? (map #(= %1 %2) ma mb))))

(defn mirror? [part mirrors rmirrors line]
  (let [start (take-last line rmirrors)
        end (drop line mirrors)
        diffs (map count-differences start end)]
    (case part
      1 (every? zero? diffs)
      2 (= 1 (reduce + diffs)))))

(defn find-mirror-line [part mirrors]
  (let [rmirrors (rseq mirrors)]
    (some #(when (mirror? part mirrors rmirrors %)
             %)
          (range 1 (count mirrors)))))

(defn transpose-mirrors [mirrors]
  (mapv #(mapv (fn [m] (nth m %)) mirrors) (range (count (first mirrors)))))

(defn find-mirror [part mirrors]
  (if-let [horizontal (find-mirror-line part mirrors)]
    (* 100 horizontal)
    (find-mirror-line part (transpose-mirrors mirrors))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (let [patterns (str/split input #"\n\n")]
    (mapv #(into [] (mapv (fn [l] (into [] l)) (str/split-lines %))) patterns)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (transduce (map #(find-mirror 1 %))
             +
             input))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (transduce (map #(find-mirror 2 %))
             +
             input))
