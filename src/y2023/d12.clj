(ns y2023.d12
  (:require [clojure.core.memoize :as memo]
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2023/day/12

;; Generator Logic

(defn parse-line [line]
  (let [[springs damaged] (str/split line #" ")
        damaged (mapv parse-long (str/split damaged #","))]
    [(into [] springs) damaged]))

;; Solution Logic

(def get-count
  (memo/memo
   (fn [springs counts pos current-count countpos]
     (cond
       (= pos (count springs))
       (if (= (count counts) countpos) 1 0)

       (= \# (nth springs pos))
       (get-count springs counts (inc pos) (inc current-count) countpos)

       (or (= \. (nth springs pos)) (= countpos (count counts)))
       (cond
         (and (< countpos (count counts))
              (= current-count (nth counts countpos)))
         (get-count springs counts (inc pos) 0 (inc countpos))

         (zero? current-count)
         (get-count springs counts (inc pos) 0 countpos)

         :else
         0)

       :else
       (let [broken-count (get-count springs counts (inc pos) (inc current-count) countpos)
             working-count (cond
                             (= current-count (nth counts countpos))
                             (get-count springs counts (inc pos) 0 (inc countpos))

                             (zero? current-count)
                             (get-count springs counts (inc pos) 0 countpos)

                             :else
                             0)]
         (+ broken-count working-count))))))

;; Entry Points

(defn generator
  " The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns "
  [input]
  (->> input
       str/split-lines
       (mapv parse-line)))

(defn solve-part-1
  " The solution to part 1. Will be called with the result of the generator "
  [input]
  (transduce
   (map (fn [[springs counts]]
          (get-count (conj springs \.) counts 0 0 0)))
   +
   input))

(defn solve-part-2
  " The solution to part 2. Will be called with the result of the generator "
  [input]
  (transduce (comp
              (map (fn [[springs counts]]
                     [(into [] (flatten (interpose \? (repeat 5 springs))))
                      (into [] (flatten (repeat 5 counts)))]))
              (map (fn [[springs counts]]
                     (get-count (conj springs \.) counts 0 0 0))))
             +
             input))

