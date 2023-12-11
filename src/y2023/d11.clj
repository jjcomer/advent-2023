(ns y2023.d11
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2023/day/11

;; Generator Logic

(defn parse-star-chart [lines]
  (let [max-x (count (first lines))
        galaxies (reduce (fn [acc [y xs]]
                           (reduce (fn [acc [x c]]
                                     (if (= c \#)
                                       (conj acc [x y])
                                       acc))
                                   acc
                                   (map-indexed vector xs)))
                         #{}
                         (map-indexed vector lines))
        y-expansions (reduce (fn [acc [y line]]
                               (if (re-find #"#" line)
                                 acc
                                 (conj acc y)))
                             #{}
                             (map-indexed vector lines))
        x-expansions (reduce (fn [acc [x column]]
                               (if (column \#)
                                 acc
                                 (conj acc x)))
                             #{}
                             (map-indexed vector (map (fn [x] (into #{} (map #(nth % x) lines)))
                                                      (range max-x))))]
    [galaxies y-expansions x-expansions]))

;; Solution Logic

(defn gen-range [p1 p2]
  (if (< p1 p2)
    (range p1 p2)
    (range p2 p1)))

(defn compute-distance [[g1x g1y] [g2x g2y] y-expansions x-expansions factor]
  (+ (abs (- g1x g2x))
     (abs (- g1y g2y))
     (* factor (count (filter x-expansions (gen-range g1x g2x))))
     (* factor (count (filter y-expansions (gen-range g1y g2y))))))

(defn gen-pairs [galaxies]
  (into #{} (for [a galaxies
                  b galaxies
                  :when (not= a b)]
              #{a b})))

;; Entry Points

(defn generator
  " The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns "
  [input]
  (parse-star-chart (str/split-lines input)))

(defn solve-part-1
  " The solution to part 1. Will be called with the result of the generator "
  [[galaxies y-expansions x-expansions]]
  (transduce (map (fn [gs]
                    (compute-distance (first gs) (second gs) y-expansions x-expansions 1)))
             +
             (gen-pairs galaxies)))

(defn solve-part-2
  " The solution to part 2. Will be called with the result of the generator "
  [[galaxies y-expansions x-expansions]]
  (transduce (map (fn [gs]
                    (compute-distance (first gs) (second gs) y-expansions x-expansions (dec 1000000))))
             +
             (gen-pairs galaxies)))
