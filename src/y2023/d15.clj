(ns y2023.d15
  (:require [clojure.string :as str]
            [flatland.ordered.map :as om]))

;; PROBLEM LINK https://adventofcode.com/2023/day/15

;; Generator Logic

(def reg #"^(\w*)(.)(\d+)?$")

(defn parse-code [line]
  (let [[raw code op val] (re-matches reg line)
        result {:raw raw :code code :op (keyword op)}]
    (if (= :- (:op result))
      result
      (assoc result :value (parse-long val)))))

;; Solution Logic

(defn gen-hash [s]
  (reduce (fn [h c]
            (mod (* 17 (+ h (int c))) 256))
          0
          s))

(defn generate-map [ops]
  (reduce (fn [the-map {:keys [code op value]}]
            (let [index (gen-hash code)]
              (case op
                :- (update the-map index dissoc code)
                := (update the-map index assoc code value))))
          (into [] (repeat 256 (om/ordered-map)))
          ops))

(defn sum-box [[box lenses]]
  (transduce (comp
              (map-indexed vector)
              (map (fn [[lens power]]
                     (* (inc box) (inc lens) (val power)))))
             +
             lenses))

(defn focus-power [the-map]
  (transduce (comp
              (map-indexed vector)
              (map sum-box))
             +
             the-map))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (let [lines (-> input
                  str/trim
                  (str/split #","))]
    (mapv parse-code lines)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (transduce (comp (map :raw)
                   (map gen-hash))
             +
             input))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (-> input
      generate-map
      focus-power))
