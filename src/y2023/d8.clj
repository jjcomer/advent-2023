(ns y2023.d8
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as m]))

;; PROBLEM LINK https://adventofcode.com/2023/day/8

;; Generator Logic

(def map-regex #"^(\w{3}) = \((\w{3}), (\w{3})\)$")

(defn parse-maps [maps]
  (reduce (fn [acc current-map]
            (let [[_ node left right] (re-matches map-regex current-map)]
              (assoc acc node [left right])))
          {}
          (str/split-lines maps)))

;; Solution Logic

(defn find-path [directions maps start end?]
  (loop [steps 0 current-node start directions (cycle directions)]
    (if (end? current-node)
      steps
      (let [[left right] (get maps current-node)
            next-node (case (first directions)
                        \L left \R right)]
        (recur (inc steps) next-node (rest directions))))))

(defn find-starts [nodes]
  (into [] (filter #(str/ends-with? % "A")) nodes))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (let [[directions maps] (str/split input #"\n\n")]
    [(into [] directions)
     (parse-maps maps)]))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [[directions maps]]
  (find-path directions maps "AAA" #(= "ZZZ" %)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [[directions maps]]
  (let [paths (map (fn [start] (find-path directions
                                          maps
                                          start
                                          #(str/ends-with? % "Z")))
                   (find-starts (keys maps)))]
    (reduce m/lcm paths)))
