(ns y2023.d17
  (:require [clojure.data.priority-map :as p]
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2023/day/17

;; Generator Logic

(defn parse-grid [rows]
  (->> rows
       (map-indexed vector)
       (reduce (fn [grid [row columns]]
                 (reduce (fn [grid [column ^Character n]]
                           (assoc! grid [row column] (- (int n) 48)))
                         grid
                         (map-indexed vector columns)))
               (transient {}))
       persistent!))

;; Solution Logic

(def deltas {:right [0 1] :left [0 -1] :up [-1 0] :down [1 0]})

(defn turn [direction]
  (case direction
    (:up :down) [:left :right]
    (:left :right) [:up :down]))

(defn move [position direction]
  (let [[r c] position
        [dr dc] (deltas direction)]
    [(+ dr r) (+ dc c)]))

(defn min-heat-loss [grid goal min-straight max-straight]
  (loop [queue (p/priority-map
                [[0 1] :right 1] 0
                [[1 0] :down 1] 0)
         cache {}]
    (if-let [[[position direction current-straight :as cache-key] heatloss] (peek queue)]
      (if (not (contains? grid position))
        (recur (pop queue) cache)
        (let [new-heatloss (+ heatloss (get grid position))]
          (if (= goal position)
            new-heatloss
            (if (and (contains? cache cache-key)
                     (<= (get cache cache-key) new-heatloss))
              (recur (pop queue) cache)
              (let [new-cache (assoc cache cache-key new-heatloss)
                    straight (if (< current-straight max-straight)
                               {[(move position direction) direction (inc current-straight)] new-heatloss}
                               {})
                    turns (if (>= current-straight min-straight)
                            (reduce (fn [acc d]
                                      (assoc acc [(move position d) d 1] new-heatloss))
                                    {}
                                    (turn direction))
                            {})]
                (recur (merge-with min (pop queue) straight turns)
                       new-cache))))))
      -1)))


;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (let [lines (str/split-lines input)
        grid (parse-grid lines)
        height (count lines)
        width (count (first lines))]
    [grid height width]))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [[grid height width]]
  (min-heat-loss grid [(dec height) (dec width)] 1 3))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [[grid height width]]
  (min-heat-loss grid [(dec height) (dec width)] 4 10))
