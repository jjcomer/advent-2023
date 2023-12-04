(ns y2023.d4
  (:require [clojure.set :as set]
            [clojure.math :as math]
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2023/day/4

;; Generator Logic

(defn parse-numbers [numbers]
  (mapv parse-long (str/split (str/trim numbers) #" +")))

(defn parse-card [line]
  (let [[card numbers] (str/split line #": ")
        card (-> card (str/split #" +") second parse-long)
        [found winning] (str/split numbers #" \| ")]
    [card (parse-numbers found) (parse-numbers winning)]))

;; Solution Logic

(defn find-score [found winning]
  (let [matched-numbers (set/intersection (set found) (set winning))]
    (if (empty? matched-numbers)
      0
      (int (math/pow 2 (dec (count matched-numbers)))))))

(defn find-score2 [cards]
  (loop [total-cards (into [] (repeat (count cards) 1))
         cards cards]
    (if-let [[i found winning] (first cards)]
      (let [score (count (set/intersection (set found) (set winning)))
            number-of-current-card (nth total-cards (dec i))]
        (recur (reduce (fn [acc i]
                         (update acc i + number-of-current-card))
                       total-cards
                       (filter #(< % (count total-cards)) (range i (+ i score))))
               (rest cards)))
      (reduce + total-cards))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input str/trim str/split-lines (mapv parse-card)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (transduce (map (fn [[_ found winning]] (find-score found winning)))
             +
             input))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (find-score2 input))
