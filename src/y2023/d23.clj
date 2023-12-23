(ns y2023.d23
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2023/day/23

;; Solution Logic

(defn solve-maze [maze height width row col end-row end-col visited length]
  (cond
    (and (= row end-row) (= col end-col)) length
    (visited [row col]) -1
    :else
    (let [new-visited (conj visited [row col])
          offsets (case (get-in maze [row col])
                    \< [[0 -1]]
                    \> [[0 1]]
                    \^ [[-1 0]]
                    \v [[1 0]]
                    [[-1 0] [1 0] [0 -1] [0 1]])
          next-locations (keep (fn [[dr dc]]
                                 (let [nr (+ dr row)
                                       nc (+ dc col)]
                                   (when (and (>= nc 0)
                                              (>= nr 0)
                                              (< nr height)
                                              (< nc width)
                                              (not= \# (get-in maze [nr nc]))
                                              (not (new-visited [nr nc])))
                                     [nr nc])))
                               offsets)]
      (if (empty? next-locations)
        -1
        (transduce (map (fn [[nr nc]]
                          (solve-maze maze height width nr nc end-col end-row new-visited (inc length))))
                   max
                   -1
                   next-locations)))))

(def offsets [[-1 0] [1 0] [0 -1] [0 1]])

(defn solve-maze2 [maze height width row col end-row end-col visited length]
  (cond
    (and (= row end-row) (= col end-col)) length
    (visited [row col]) -1
    :else
    (let [new-visited (conj visited [row col])
          next-locations (keep (fn [[dr dc]]
                                 (let [nr (+ dr row)
                                       nc (+ dc col)]
                                   (when (and (>= nc 0)
                                              (>= nr 0)
                                              (< nr height)
                                              (< nc width)
                                              (not= \# (get-in maze [nr nc]))
                                              (not (new-visited [nr nc])))
                                     (loop [run-length 1
                                            visited new-visited
                                            rr nr
                                            rc nc]
                                       (let [possible-directions (keep (fn [[dr dc]]
                                                                         (let [nr (+ dr rr)
                                                                               nc (+ dc rc)]
                                                                           (when (and (>= nr 0)
                                                                                      (>= nc 0)
                                                                                      (< nr height)
                                                                                      (< nc width)
                                                                                      (not (visited [nr nc]))
                                                                                      (not= \# (get-in maze [nr nc])))
                                                                             [nr nc])))
                                                                       offsets)]
                                         (if (= 1 (count possible-directions))
                                           (let [[nr nc] (first possible-directions)]
                                             (recur (inc run-length) (conj visited [rr rc]) nr nc))
                                           [run-length visited rr rc]))))))
                               offsets)]
      (if (empty? next-locations)
        -1
        (transduce (map (fn [[run-length visited nr nc]]
                          (solve-maze2 maze height width nr nc end-col end-row visited (+ run-length length))))
                   max
                   -1
                   next-locations)))))

(defn find-start-and-end [maze]
  (let [start-col (str/index-of (str/join (first maze)) \.)
        end-col (str/last-index-of (str/join (last maze)) \.)]
    [[0 start-col]
     [(dec (count maze)) end-col]]))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (let [maze (mapv (fn [line]
                     (into [] line))
                   (str/split-lines input))
        height (count maze)
        width (count (first maze))]
    [maze height width]))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [[maze height width]]
  (let [[[sr sc] [er ec]] (find-start-and-end maze)]
    (solve-maze maze height width sr sc er ec #{} 0)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [[maze height width]]
  (let [[[sr sc] [er ec]] (find-start-and-end maze)]
    (solve-maze2 maze height width sr sc er ec #{} 0)))
