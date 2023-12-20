(ns y2023.d20
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2023/day/20

;; Generator Logic

(defn parse-lines2 [lines]
  (let [mods (map (fn [line]
                    (let [[label dests] (str/split line #" -> ")
                          dests (str/split dests #", ")]
                      (if (= "broadcaster" label)
                        {:type \b :label label :dests dests :state false}
                        {:type (first label) :label (subs label 1) :dests dests :state false}))) lines)
        mods (conj mods
                   {:type \o :label "output" :dests [] :state false}
                   {:type \o :label "rx" :dests [] :state false})
        mods (reduce #(assoc %1 (:label %2) %2) {} mods)
        mods (reduce (fn [mods mod]
                       (reduce (fn [mods dest]
                                 (assoc-in mods [dest :inputs (:label mod)] false))
                               mods
                               (:dests mod)))
                     mods
                     (vals mods))
        parent (first (keys (get-in mods ["rx" :inputs] {})))
        watch-mods (into #{} (keys (get-in mods [parent :inputs])))]
    [mods parent watch-mods]))

;; Solution Logic

(def empty-queue clojure.lang.PersistentQueue/EMPTY)

(defn add-dests [queue {:keys [label dests state]}]
  (reduce (fn [queue d]
            (conj queue [label d state]))
          queue
          dests))

(defn simulate-part1 [mods]
  (loop [counter (range 1000)
         mods mods
         highs 0
         lows 0]
    (if (first counter)
      (let [[mods highs lows]
            (loop [mods mods
                   highs highs
                   lows lows
                   queue (conj empty-queue ["button" "broadcaster" false])]
              (if-let [[source label pulse] (peek queue)]
                (let [[highs lows] (if pulse [(inc highs) lows] [highs (inc lows)])
                      current-mod (get mods label)]
                  (case (:type current-mod)
                    \% (if pulse
                         (recur mods highs lows (pop queue))
                         (let [mods (update-in mods [label :state] not)]
                           (recur mods highs lows
                                  (add-dests (pop queue) (get mods label)))))
                    \& (let [mods (assoc-in mods [label :inputs source] pulse)
                             mods (assoc-in mods [label :state]
                                            (not (every? identity (vals (get-in mods [label :inputs])))))]
                         (recur mods highs lows
                                (add-dests (pop queue) (get mods label))))
                    (recur mods highs lows
                           (add-dests (pop queue) (get mods label)))))
                [mods highs lows]))]
        (recur (rest counter) mods highs lows))
      (* highs lows))))

(defn simulate-part-2 [mods parent watches]
  (loop [counter 0
         watches (zipmap watches (repeat nil))
         mods mods]
    (if (every? identity (vals watches))
      (reduce * (vals watches))
      (let [i (inc counter)
            [mods watches]
            (loop [mods mods
                   watches watches
                   queue (conj empty-queue ["button" "broadcaster" false])]
              (if-let [[source label pulse] (peek queue)]
                (let [current-mod (get mods label)]
                  (case (:type current-mod)
                    \% (if pulse
                         (recur mods watches (pop queue))
                         (let [mods (update-in mods [label :state] not)]
                           (recur mods watches
                                  (add-dests (pop queue) (get mods label)))))
                    \& (let [mods (assoc-in mods [label :inputs source] pulse)
                             mods (assoc-in mods [label :state]
                                            (not (every? identity (vals (get-in mods [label :inputs])))))]
                         (recur mods
                                (if (and (= parent label)
                                         (contains? watches source)
                                         pulse
                                         (not (get watches source)))
                                  (assoc watches source i)
                                  watches)
                                (add-dests (pop queue) (get mods label))))
                    (recur mods watches
                           (add-dests (pop queue) (get mods label)))))
                [mods watches]))]
        (recur i watches mods)))))

(comment
  "There are four gates that will turn on rs (based on my input) which then turns on rx: bt, dl, fr, rv")

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (parse-lines2 (str/split-lines input)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [[mods _ _]]
  (simulate-part1 mods))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [[mods parents watches]]
  (simulate-part-2 mods parents watches))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(def s1 "broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a")

(deftest p1-sample-1
  (let [input (generator s1)
        result (solve-part-1 input)]
    (t/is (= 32000000 result))))

(def s2 "broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output")

(deftest p1-sample-2
  (let [input (generator s2)
        result (solve-part-1 input)]
    (t/is (= 11687500 result))))