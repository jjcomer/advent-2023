(ns y2023.d7
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2023/day/7

;; Generator Logic

(def cards (zipmap [\2 \3 \4 \5 \6 \7 \8 \9 \T \J \Q \K \A] (range 13)))
(def cards2 (zipmap [\J \2 \3 \4 \5 \6 \7 \8 \9 \T \Q \K \A] (range 13)))
(def hands {[1 1 1 1 1] 0
            [1 1 1 2] 1
            [1 2 2] 2
            [1 1 3] 3
            [2 3] 4
            [1 4] 5
            [5] 6})

;; Solution Logic

(defn hand-score [hand]
  (let [id (->> hand
                (group-by identity)
                vals
                (map count)
                sort
                (into []))]
    (get hands id 0)))

(defn hand-score2 [hand]
  (let [card-groups (group-by identity hand)
        jokers (count (get card-groups \J []))
        card-groups (->> (dissoc card-groups \J)
                         vals
                         (map count)
                         sort
                         (into []))]
    (if (empty? card-groups)
      6
      (get hands (update card-groups (dec (count card-groups)) + jokers) 0))))

(defn compare-hands [score card a b]
  (let [c (compare (score a) (score b))]
    (if (zero? c)
      (compare (mapv card a) (mapv card b))
      c)))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input
       str/trim
       str/split-lines
       (map #(str/split % #" "))
       (map #(update % 1 parse-long))
       (into [])))

(defn score-hand [rank [_ bid]]
  (* (inc rank) bid))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (->> input
       (sort-by first (partial compare-hands hand-score cards))
       (map-indexed score-hand)
       (reduce +)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (->> input
       (sort-by first (partial compare-hands hand-score2 cards2))
       (map-indexed score-hand)
       (reduce +)))

