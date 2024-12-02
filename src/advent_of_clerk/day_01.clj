;; # 🎄 Advent of Clerk: Day 1
(ns advent-of-clerk.day-01
  (:require [nextjournal.clerk :as clerk]
            [advent-of-clerk.utils :as utils]
            [clojure.string :as str]))


(def input (utils/load-input "day_01.txt"))

(def ex1
  "3   4
4   3
2   5
1   3
3   9
3   3
")

(defn parse-input
  [input]
  (let [xs (->> (re-seq #"\d+" input)
                (map read-string))]
    [(take-nth 2 xs)
     (take-nth 2 (rest xs))]))

;; Parsing the left and right columns:
(def parsed-ex1 (parse-input ex1))

;; Sorting each column and taking the difference:
(->> parsed-ex1
     (map sort)
     (apply map (fn [a b] (Math/abs (- a b)))))


;; ---
;; ## Part 1

(defn solve-1
  [input]
  (->> input
       parse-input
       (map sort)
       (apply map (fn [a b] (Math/abs (- a b))))
       (reduce +)))

(= (solve-1 ex1) 11)
(= (solve-1 input) 1197984)

;; ---
;; ## Part 2

(defn solve-2
  [input]
  (->> input))


(comment

  ;;
  )
