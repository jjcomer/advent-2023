(ns y2023.d14
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;; PROBLEM LINK https://adventofcode.com/2023/day/14

;; Solution Logic

(defn flip-vec [v]
  [(second v) (first v)])

(defn prep-rocks [rocks direction-rows direction-columns]
  (cond
    (= -1 direction-rows)
    (sort rocks)

    (= 1 direction-rows)
    (reverse (sort rocks))

    (= -1 direction-columns)
    (sort-by flip-vec rocks)

    (= 1 direction-columns)
    (reverse (sort-by flip-vec rocks))))

(defn roll-rocks [cubes height width rocks direction-rows direction-columns]
  (let [new-grid (reduce (fn [grid [r c]]
                           (loop [new-r (+ r direction-rows)
                                  new-c (+ c direction-columns)]
                             (if (and (< -1 new-r height)
                                      (< -1 new-c width)
                                      (not (grid [new-r new-c])))
                               (recur (+ new-r direction-rows)
                                      (+ new-c direction-columns))
                               (conj grid [(- new-r direction-rows)
                                           (- new-c direction-columns)]))))
                         cubes
                         (prep-rocks rocks direction-rows direction-columns))]
    (set/difference new-grid cubes)))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (let [input (str/split-lines input)
        [cubes rocks] (reduce (fn [[cubes rocks] [r row]]
                                (reduce (fn [[cubes rocks :as acc] [c x]]
                                          (case x
                                            \# [(conj cubes [r c]) rocks]
                                            \O [cubes (conj rocks [r c])]
                                            acc))
                                        [cubes rocks]
                                        (map-indexed vector row)))
                              [#{} #{}]
                              (map-indexed vector input))
        height (count input)
        width (count (first input))]
    [cubes rocks height width]))

(defn perform-cycle [cubes height width rocks]
  (let [directions [[-1 0] [0 -1] [1 0] [0 1]]]
    (reduce (fn [rocks [direction-rows direction-columns]]
              (roll-rocks cubes height width rocks direction-rows direction-columns))
            rocks
            directions)))

(defn perform-all-the-cycles [cubes height width rocks]
  (loop [seen {}
         r-seen {}
         rocks rocks
         i (range 1 1000000000)]
    (let [new-rocks (perform-cycle cubes height width rocks)
          current-cycle (first i)]
      (if (contains? seen new-rocks)
        (let [cycle-start (get seen new-rocks)
              cycle-length (- current-cycle cycle-start)
              offset (- current-cycle cycle-length)
              remaining (- 1000000000 cycle-start)
              final-lookup (+ (mod remaining cycle-length) offset)]
          (get r-seen final-lookup))
        (recur (assoc seen new-rocks current-cycle)
               (assoc r-seen current-cycle new-rocks)
               new-rocks
               (rest i))))))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [[cubes rocks height width]]
  (let [roll-north (roll-rocks cubes height width rocks -1 0)]
    (transduce (comp (map first)
                     (map #(- height %)))
               +
               roll-north)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [[cubes rocks height width]]
  (let [cycled (perform-all-the-cycles cubes height width rocks)]
    (transduce (comp (map first)
                     (map #(- height %)))
               +
               cycled)))
