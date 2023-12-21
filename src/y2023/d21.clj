(ns y2023.d21
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2023/day/21

;; Generator Logic

(defn parse-grid [lines]
  (let [[start rocks] (reduce (fn [acc [r row]]
                                (reduce (fn [[start rocks :as acc] [c x]]
                                          (case x
                                            \S [[r c] rocks]
                                            \# [start (conj rocks [r c])]
                                            acc))
                                        acc
                                        (map-indexed vector row)))
                              [nil #{}]
                              (map-indexed vector lines))
        height (count lines)
        width (count (first lines))]
    [rocks start height width]))

;; Solution Logic

(def deltas [[0 1] [1 0] [-1 0] [0 -1]])

(defn take-steps [rocks height width start steps]
  (loop [steps (range 1 steps)
         gardens #{start}]
    (if (first steps)
      (let [new-gardens (reduce (fn [acc [r c]]
                                  (reduce (fn [acc [dr dc]]
                                            (let [new-r (+ r dr)
                                                  new-c (+ c dc)]
                                              (if (not (rocks [(mod new-r height) (mod new-c width)]))
                                                (conj acc [new-r new-c])
                                                acc)))
                                          acc
                                          deltas))
                                #{}
                                gardens)]
        (recur (rest steps) new-gardens))
      gardens)))

(defn take-steps2 [rocks height width start]
  (let [total-steps 26501365]
    (loop [steps (range 1 total-steps)
           gardens #{start}
           points {}]
      (if-let [s (first steps)]
        (let [new-gardens (reduce (fn [acc [r c]]
                                    (reduce (fn [acc [dr dc]]
                                              (let [new-r (+ r dr)
                                                    new-c (+ c dc)]
                                                (if (not (rocks [(mod new-r height) (mod new-c width)]))
                                                  (conj acc [new-r new-c])
                                                  acc)))
                                            acc
                                            deltas))
                                  #{}
                                  gardens)
              points (if (= (mod s height) (mod total-steps height))
                       (assoc points (quot s height) (count new-gardens))
                       points)]
          (if (= 3 (count points))
            (let [y0 (get points 0)
                  y1 (get points 1)
                  y2 (get points 2)
                  n (quot total-steps height)
                  a (/ (- (+ y2 y0) (* 2 y1)) 2)
                  b (- y1 y0 a)]
              (+ y0 (* b n) (* a n n)))
            (recur (rest steps) new-gardens points)))
        gardens))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (parse-grid (str/split-lines input)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [[rocks start height width]]
  (count (take-steps rocks height width start 65)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [[rocks start height width]]
  (take-steps2 rocks height width start))
