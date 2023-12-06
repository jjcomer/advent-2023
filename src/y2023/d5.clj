(ns y2023.d5
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2023/day/5

;; Generator Logic

(defn parse-seeds [seeds]
  (mapv parse-long (str/split (subs seeds 7) #" ")))

(defn parse-range [raw-range]
  (let [[start-1 start-2 length] (map parse-long (str/split raw-range #" "))]
    [start-2 (+ start-2 length) (- start-1 start-2)]))

(defn parse-map [raw-map]
  (let [[_ & ranges] (str/split-lines raw-map)]
    (into [] (sort-by first (map parse-range ranges)))))

;; Solution Logic

(defn check-range [v ranges]
  (reduce (fn [v [start end offset]]
            (if (and (<= start v) (< v end))
              (reduced (+ v offset))
              v))
          v
          ranges))

(defn find-location [maps v]
  (reduce check-range v maps))

(defn check-range-2 [start end ranges]
  (let [current-start (atom start)
        result (atom [])]
    (reduce (fn [_ [r-start r-end offset]]
              (when (> r-start @current-start)
                (swap! result conj [@current-start (min end r-start)])
                (reset! current-start r-start))
              (if (>= @current-start r-end)
                (reduced nil)
                (do
                  (swap! result conj [(+ @current-start offset)
                                      (+ offset (min end r-end))])
                  (reset! current-start r-end)
                  (when (>= @current-start end)
                    (reduced nil)))))
            nil
            (remove #(< (second %) start) ranges))
    (when (< @current-start end)
      (swap! result conj [@current-start end]))
    @result))

(defn find-location-2 [maps seed-start seed-length]
  (reduce (fn [ranges current-map]
            (mapcat #(check-range-2 (first %) (second %) current-map) ranges))
          [[seed-start (+ seed-start seed-length)]]
          maps))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (let [[seeds & maps] (str/split input #"\n\n")]
    [(parse-seeds seeds)
     (mapv parse-map maps)]))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [[seeds maps]]
  (reduce min (map #(find-location maps %) seeds)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [[seeds maps]]
  (reduce (fn [acc [seed-start seed-length]]
            (let [ranges (find-location-2 maps seed-start seed-length)]
              (min acc (reduce min (map first ranges)))))
          Long/MAX_VALUE
          (partition 2 seeds)))

