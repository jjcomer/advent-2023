(ns y2023.d18
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2023/day/18

;; Generator Logic

(defn parse-line [line]
  (let [[direction length part2] (str/split line #" ")
        p2-direction (- (int (nth part2 (- (count part2) 2))) 48)
        p2-distance (Integer/parseInt (subs part2 2 (- (count part2) 2)) 16)]
    [[(keyword direction) (parse-long length)] [p2-direction p2-distance]]))

;; Solution Logic

(def p1-directions
  {:R [0 1] :L [0 -1] :D [1 0] :U [-1 0]})

(def p2-directions
  {0 [0 1] 2 [0 -1] 1 [1 0] 3 [-1 0]})

(defn polygon-area [vertices]
  (let [v (cons (last vertices) vertices)
        sum (reduce (fn [acc [[x1 y1] [x2 y2]]]
                      (+ acc
                         (- (* x2 y1)
                            (* x1 y2))))
                    0
                    (partition 2 1 v))]
    (* 0.5 (abs sum))))

(defn interior-points [area border-size]
  (long (+ 1 (- area (* 0.5 border-size)))))

(defn find-polygon [d-lookup steps]
  (let [border-size (transduce (map second) + steps)
        vertcies (loop [row 0 col 0 vertcies [] steps steps]
                   (if-let [[direction distance] (first steps)]
                     (let [[dr dc] (get d-lookup direction)
                           new-row (+ row (* dr distance))
                           new-col (+ col (* dc distance))]
                       (recur new-row
                              new-col
                              (conj vertcies [new-row new-col])
                              (rest steps)))
                     vertcies))
        area (polygon-area vertcies)
        interior (interior-points area border-size)]
    (+ interior border-size)))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input
       str/split-lines
       (mapv parse-line)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (->> input
       (map first)
       (find-polygon p1-directions)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (->> input
       (map second)
       (find-polygon p2-directions)))
