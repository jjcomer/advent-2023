(ns y2023.d22
  (:require [clojure.set :as set]
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2023/day/22

;; Generator Logic

(defn parse-bricks [lines]
  (mapv (fn [line]
          (let [[x1 y1 z1 x2 y2 z2] (map parse-long (str/split line #"[,~]"))]
            (into #{} (for [x (range x1 (inc x2))
                            y (range y1 (inc y2))
                            z (range z1 (inc z2))]
                        [x y z]))))
        lines))

;; Solution Logic

(defn settle-bricks [all-points bricks]
  (let [[i dropped new-all-points new-bricks]
        (reduce (fn [[i dropped all-points bricks] brick]
                  (let [all-without (set/difference all-points brick)]
                    (if (every? (every-pred #(< 1 (nth % 2))
                                            #(not (all-without (update % 2 dec))))
                                brick)
                      (let [new-brick (into #{} (map #(update % 2 dec)) brick)]
                        [(inc i) true (set/union all-without new-brick) (conj bricks new-brick)])
                      [(inc i) dropped all-points (conj bricks brick)])))
                [0 false all-points []]
                bricks)]
    (if (not dropped)
      new-bricks
      (recur new-all-points new-bricks))))

(defn find-dependents [bricks]
  (map-indexed (fn [i brick-a]
                 (reduce (fn [acc [j brick-b]]
                           (if (and (not= i j)
                                    (some #(brick-b (update % 2 dec)) brick-a))
                             (conj acc j)
                             acc))
                         #{}
                         (map-indexed vector bricks)))
               bricks))

(defn find-chain-reactions [dependents on-floor to-remove]
  (let [unsupported (into #{} (comp (filter #(and (empty? (set/difference (second %) to-remove))
                                                  (not (contains? (set/union on-floor to-remove) (first %)))))
                                    (map first))
                          (map-indexed vector dependents))]
    (if (empty? unsupported)
      0
      (+ (count unsupported) (find-chain-reactions dependents on-floor (set/union to-remove unsupported))))))
;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (parse-bricks (str/split-lines input)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [bricks]
  (let [settled (settle-bricks (reduce set/union bricks) bricks)
        dependents (find-dependents settled)]
    (- (count settled) (count (filter #(= 1 (count %)) dependents)))))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [bricks]
  (let [settled (settle-bricks (reduce set/union bricks) bricks)
        dependents (find-dependents settled)
        on-floor (into #{} (comp (filter (fn [[_ d]] (empty? d)))
                                 (map first))
                       (map-indexed vector dependents))]
    (transduce (map #(find-chain-reactions dependents on-floor #{%}))
               +
               (range (count bricks)))))
