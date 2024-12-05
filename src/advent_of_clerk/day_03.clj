;; # 🎄 Advent of Clerk: Day 3
(ns advent-of-clerk.template
  (:require [nextjournal.clerk :as clerk]
            [advent-of-clerk.utils :as utils]))

(def input (utils/load-input "day_03.txt"))

(def parse-mul-args #(map read-string (re-seq #"\d+" %)))

;; ---
;; ## Part 1

(def ex1
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(defn parse-input
  [input]
  (map parse-mul-args
       (re-seq #"mul\(\d{1,3}\,\d{1,3}\)" input)))

(defn solve-1
  [input]
  (->> (parse-input input)
       (reduce (fn [sum [a b]] (+ sum (* a b))) 0)))

(= (solve-1 ex1) 161)
(= (solve-1 input) 175015740)

;; ### Experiments

(def mul-args (map parse-mul-args
                   (re-seq #"mul\(\d{1,3}\,\d{1,3}\)" ex1)))

(reduce (fn [sum [a b]] (+ sum (* a b)))
        0
        mul-args)

;; ### Observations

;; My first parsing solution gave me a list of the whole match:
(re-seq #"mul\(\d{1,3}\,\d{1,3}\)" ex1)

;; But I could have used capture groups to get the individual numbers:
(re-seq #"mul\((\d{1,3})\,(\d{1,3})\)" ex1)

;; Which would have saved me a second `re-seq` call.

;; Could have used this instead of `read-string`:
(parse-long "42")

;; All combined in an alternative approach with transduce:
(transduce (map (fn [[_ x y]] (* (parse-long x) (parse-long y))))
           +
           (re-seq #"mul\((\d+),(\d+)\)" ex1))

;; ---
;; ## Part 2

(def ex2
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defn parse-instructions
  [input]
  (re-seq #"do\(\)|don't\(\)|mul\(\d{1,3}\,\d{1,3}\)" input))

(defn run-program
  [instructions]
  (first (reduce (fn [[sum enabled?] instr]
                   (case (first instr)
                     \d (if (= "do()" instr)
                          [sum true]
                          [sum false])
                     (if enabled?
                       (let [[a b] (parse-mul-args instr)]
                         [(+ sum (* a b)) enabled?])
                       [sum enabled?])))
                 [0 true]
                 instructions)))

(defn solve-2
  [input]
  (run-program (parse-instructions input)))

(= (solve-2 ex2) 48)
(= (solve-2 input) 112272912)

;; ### Experiments

;; Parsing all instructions:
(def instructions (re-seq #"do\(\)|don't\(\)|mul\(\d{1,3}\,\d{1,3}\)" ex2))

;; Scanning instructions, while adding up the sum in memory
(reduce (fn [[sum enabled?] instr]
          (case (first instr)
            \d (if (= "do()" instr)
                 [sum true]
                 [sum false])
            (if enabled?
              (let [[a b] (parse-mul-args instr)]
                [(+ sum (* a b)) enabled?])
              [sum enabled?])))
        [0 true]
        instructions)

(comment
  
  ,)

