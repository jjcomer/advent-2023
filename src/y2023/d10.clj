(ns y2023.d10
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;; PROBLEM LINK https://adventofcode.com/2023/day/10

(defn pipe-neighbours [pipe [y x]]
  (case pipe
    \- #{[y (inc x)] [y (dec x)]}
    \| #{[(dec y) x] [(inc y) x]}
    \L #{[(dec y) x] [y (inc x)]}
    \J #{[(dec y) x] [y (dec x)]}
    \7 #{[(inc y) x] [y (dec x)]}
    \F #{[(inc y) x] [y (inc x)]}))

(defn find-neighbours [pipes coords]
  (pipe-neighbours (pipes coords) coords))

;; Generator Logic

(defn parse-map [lines]
  (reduce (fn [acc [y line]]
            (merge acc
                   (reduce (fn [acc [x pipe]]
                             (if (not= \. pipe)
                               (assoc acc [y x] pipe)
                               acc))
                           {}
                           (map-indexed vector line))))
          {}
          (map-indexed vector (str/split-lines lines))))

(defn find-start [pipes]
  (some #(when (= \S (val %)) (key %)) pipes))

(defn replace-start-pipe [pipes [start-y start-x :as start]]
  (let [[end-1 end-2] (map (fn [[y x]]
                             [(+ start-y y) (+ start-x x)])
                           (filter (fn [[y x]]
                                     (let [n [(+ y start-y) (+ x start-x)]]
                                       (and (contains? pipes n)
                                            (contains? (find-neighbours pipes n) start))))
                                   [[-1 0] [0 1] [0 -1] [1 0]]))
        end-set #{end-1 end-2}
        start-pipe (some (fn [test-pipe]
                           (when (= end-set (pipe-neighbours test-pipe start))
                             test-pipe))
                         [\- \| \L \J \7 \F])]
    (assoc pipes start start-pipe)))

;; Solution Logic

(defn walk-loop [pipes start]
  (let [start-neighbours (find-neighbours pipes start)]
    (loop [visited (set/union #{start} start-neighbours)
           end1 (first start-neighbours)
           end2 (second start-neighbours)
           steps 1]
      (if (= end1 end2)
        [visited steps]
        (let [new-end1 (first (set/difference (find-neighbours pipes end1) visited))
              new-end2 (first (set/difference (find-neighbours pipes end2) visited))]
          (recur (set/union visited #{new-end1} #{new-end2})
                 new-end1
                 new-end2
                 (inc steps)))))))

(defn find-fill [pipes the-loop max-y max-x]
  (loop [ys (range max-y) fill-count 0]
    (if-let [y (first ys)]
      (recur (rest ys)
             (loop [xs (range max-x) inside false fill-count fill-count]
               (if-let [x (first xs)]
                 (if (the-loop [y x])
                   (recur (rest xs)
                          (if (#{\| \L \J} (pipes [y x]))
                            (not inside)
                            inside)
                          fill-count)
                   (recur (rest xs)
                          inside
                          (if inside (inc fill-count) fill-count)))
                 fill-count)))
      fill-count)))

;; Entry Points

(defn generator
  " The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns "
  [input]
  (let [pipes (parse-map input)
        start-location (find-start pipes)]
    [start-location
     (replace-start-pipe pipes start-location)
     [(count (str/split-lines input)) (count (first (str/split-lines input)))]]))

(defn solve-part-1
  " The solution to part 1. Will be called with the result of the generator "
  [[start pipes _]]
  (second (walk-loop pipes start)))

(defn solve-part-2
  " The solution to part 2. Will be called with the result of the generator "
  [[start pipes [max-y max-x]]]
  (let [[the-loop _] (walk-loop pipes start)]
    (find-fill pipes the-loop max-y max-x)))
