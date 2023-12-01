(ns y2023.d1
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2023/day/1

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (-> input
      str/trim
      str/split-lines))

(defn find-code [input]
  (let [digits (re-seq #"\d" input)]
    (parse-long (str (first digits) (last digits)))))

(def first-num #"[0-9]|one|two|three|four|five|six|seven|eight|nine")
(def last-num #"^.*([0-9]|one|two|three|four|five|six|seven|eight|nine).*$")

(defn convert [n]
  (case n
    "one" 1
    "two" 2
    "three" 3
    "four" 4
    "five" 5
    "six" 6
    "seven" 7
    "eight" 8
    "nine" 9
    n))

(defn find-code2 [input]
  (let [first-digit (re-find first-num input)
        last-digit (last (re-find last-num input))]
    (parse-long (str (convert first-digit) (convert last-digit)))))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (transduce (map find-code) + input))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (transduce (map find-code2) + input))

