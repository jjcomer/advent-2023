(ns y2023.d2
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2023/day/2

;; Generator Logic

(defn parse-cubes [cubes]
  (mapv #(reduce (fn [acc cube]
                   (let [[num colour] (str/split cube #" ")]
                     (assoc acc (keyword colour) (parse-long num))))
                 {}
                 (str/split % #", "))
        (str/split cubes #"; ")))

(defn parse-game [line]
  (let [[game rounds] (str/split line #": ")
        game (-> game
                 (str/split #" ")
                 last
                 parse-long)
        rounds (parse-cubes rounds)]
    [game rounds]))

;; Solution Logic

(defn valid-game? [rounds]
  (let [max-cubes (apply merge-with max rounds)]
    (and (>= 14 (:blue max-cubes 0))
         (>= 13 (:green max-cubes 0))
         (>= 12 (:red max-cubes 0)))))

(defn gen-power [[_ rounds]]
  (let [max-cubes (apply merge-with max rounds)]
    (reduce * (vals max-cubes))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input
       str/split-lines
       (mapv parse-game)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (transduce
   (comp (filter #(valid-game? (second %)))
         (map first))
   +
   input))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (transduce (map gen-power) + input))

