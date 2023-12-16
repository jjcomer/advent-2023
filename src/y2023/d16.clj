(ns y2023.d16
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;; PROBLEM LINK https://adventofcode.com/2023/day/16

;; Generator Logic
(defn parse-grid [lines]
  (reduce (fn [grid [r row]]
            (reduce (fn [grid [c char]]
                      (assoc grid [c r] char))
                    grid
                    (map-indexed vector row)))
          {}
          (map-indexed vector lines)))

;; Solution Logic

(def right 0)
(def left 1)
(def down 2)
(def up 3)

(def deltas {[1 0] right [-1 0] left [0 1] down [0 -1] up})
(def rdeltas (set/map-invert deltas))
(def splitters {\| {left [up down] right [up down] up [up] down [down]}
                \- {up [left right] down [left right] left [left] right [right]}})
(def mirrors {\\ {right down left up down right up left}
              \/ {right up left down down left up right}})

(defn move [[pc pr] [dc dr]]
  [(+ pc dc) (+ pr dr)])

(defn fire-lasers [grid starting-position]
  (loop [beams [starting-position]
         visited (zipmap (keys grid) (repeat 0))]
    (if-let [[pos delta] (first beams)]
      (let [new-pos (move pos delta)]
        (if (contains? grid new-pos)
          (let [current-direction (get deltas delta)
                visited-state (get visited new-pos)
                direction-mask (bit-shift-left 1 current-direction)]
            (if (zero? (bit-and visited-state direction-mask))
              (let [visited (assoc visited new-pos (bit-or visited-state direction-mask))
                    mirror (get grid new-pos)]
                (cond
                  (= mirror \.)
                  (recur (conj beams [new-pos delta]) visited)

                  (contains? mirrors mirror)
                  (recur (conj beams [new-pos (get rdeltas (get-in mirrors [mirror current-direction]))])
                         visited)

                  (contains? splitters mirror)
                  (let [directions (map rdeltas (get-in splitters [mirror current-direction]))
                        new-beams (map (fn [delta] [new-pos delta]) directions)]
                    (recur (apply conj beams new-beams)
                           visited))))
              (recur (rest beams) visited)))
          (recur (rest beams) visited)))
      (count (filter pos? (vals visited))))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (let [lines (str/split-lines input)
        grid (parse-grid lines)]
    [grid (count lines) (count (first lines))]))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [[grid _ _]]
  (fire-lasers grid [[-1 0] (rdeltas right)]))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [[grid height width]]
  (let [width-starts (reduce (fn [acc column]
                               (conj acc
                                     [[column -1] (rdeltas down)]
                                     [[column height] (rdeltas up)]))
                             []
                             (range width))
        height-starts (reduce (fn [acc row]
                                (conj acc
                                      [[-1 row] (rdeltas right)]
                                      [[width row] (rdeltas left)]))
                              []
                              (range height))]
    (transduce (map #(fire-lasers grid %))
               max
               0
               (concat width-starts height-starts))))

 