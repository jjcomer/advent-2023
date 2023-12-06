(ns y2023.d6
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as str]
            [clojure.math :as math]))

;; PROBLEM LINK https://adventofcode.com/2023/day/6

;; Generator Logic

;; Solution Logic

;;This should be re-written to be cleaner
(defn quadratic [time record]
  (let [t (mod time 2)
        a (- (* time time) (* 4 record))
        b (+ (math/sqrt a) t)
        c (* 2 (long (/ b 2)))
        d (- (+ c 1) t)]
    d))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (let [[times records] (map #(map parse-long (rest (str/split % #" +"))) (str/split-lines input))]
    (map vector times records)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (transduce (map #(quadratic (first %) (second %))) * input))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (let [time (parse-long (str/join (map first input)))
        record (parse-long (str/join (map second input)))]
    (quadratic time record)))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(deftest sample-test
  (t/is (= 2 (+ 1 1))))
