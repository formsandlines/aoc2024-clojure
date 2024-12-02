;; # 🎄 Advent of Clerk: Day 2
(ns advent-of-clerk.day-02
  (:require [nextjournal.clerk :as clerk]
            [advent-of-clerk.utils :as utils]
            [clojure.string :as str]))


(def input (utils/load-input "day_02.txt"))

(def ex1
  "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
")

(defn parse-input
  [input]
  (mapv #(map read-string (str/split % #"\s+"))
        (str/split-lines input)))

;; Records of levels:
(def ex1-reports (parse-input ex1))
(def input-reports (parse-input input))

(def ex1-sample (first ex1-reports))

;; Helper functions to determine change:

(defn signum
  [x]
  (cond (< x 0) -1
        (> x 0) 1
        :else 0))

(defn compare-level-changes
  [sign next-sign]
  (cond
    (= 0 next-sign) :constant
    (or (nil? sign)
        (= next-sign sign)) :consistent
    :else :varying))

;; All test cases:

(= (compare-level-changes 1 0)
   (compare-level-changes 0 0)
   (compare-level-changes -1 0)
   :constant)

(= (compare-level-changes -1 1)
   (compare-level-changes 1 -1)
   :varying)

(= (compare-level-changes -1 -1)
   (compare-level-changes 1 1)
   :consistent)

(defn safe-report?
  "Returns true if a report is safe and throws an exception otherwise."
  [report]
  (loop [a (first report)
         r (rest report)
         sign nil] ;; -1 | 0 | 1
    (if-let [b (first r)]
      (let [diff (- b a)
            next-sign (signum diff)]
        (case (compare-level-changes sign next-sign)
          :constant (throw (ex-info (str a "-" b " is constant") {}))
          :varying (throw
                    (ex-info (str a "-" b " does not "
                                  (case sign 1 "increase" -1 "decrease" "???"))
                             {}))
          (if (> (Math/abs diff) 3)
            (throw
             (ex-info (str "difference " (Math/abs diff) " > 3") {}))
            (recur b (rest r) next-sign))))
      true)))

(defn make-reports-viewer [func]
  (fn viewer
    ([reports] (viewer reports 0))
    ([reports from-index]
     (clerk/table
      {:head ["Index" "Report" "safe?"]
       :rows (for [[index report] (map-indexed vector reports)]
               [(+ from-index index)
                report (try (func report)
                            (catch Exception e
                              (clerk/html
                               [:pre {:style {:color "red"}}
                                (.getMessage e)])))])}))))

(def show-reports (make-reports-viewer safe-report?))

(show-reports ex1-reports)

;; In the real data, no safe reports are found until record 409:
(show-reports (take 400 input-reports))

(some? (some (fn [rec] (true? (try (safe-report? rec)
                                  (catch Exception _ false))))
             (take 409 input-reports)))

;; After that, every report is safe:
(show-reports (drop 400 input-reports) 400)

(every? (fn [rec] (try (safe-report? rec)
                      (catch Exception _ false)))
        (drop 409 input-reports))

;; ---
;; ## Part 1

(defn solve-1
  [input]
  (count (remove nil?
                 (map #(try (safe-report? %) (catch Exception _ nil))
                      (parse-input input)))))

(= (solve-1 ex1) 2)
(= (solve-1 input) 591)

;; ---
;; ## Part 2

(comment
  (defn safe-report-with-dampener?
    "Returns true if a report is safe, including safe for the problem dampener, and
  throws an exception otherwise."
    [report]
    (loop [a (first report)
           r (rest report)
           sign nil ;; -1 | 0 | 1
           bad-levels 0]
      (if-let [b (first r)]
        (let [diff (- b a)
              next-sign (signum diff)
              bad-levels (if (or (not= (compare-level-changes sign next-sign)
                                       :consistent)
                                 (> (Math/abs diff) 3))
                           (inc bad-levels)
                           bad-levels)]
          (case bad-levels
            0 (recur b (rest r) next-sign bad-levels)
            1 (recur a (rest r) sign bad-levels)
            (throw (ex-info (str "at least " bad-levels " bad levels") {}))))
        true))))

(defn safe-report-with-dampener?
  "Returns true if a report is safe, including safe for the problem dampener, and
  throws an exception otherwise."
  [report]
  (loop [a (first report)
         r (rest report)
         sign nil ;; -1 | 0 | 1
         ]
    (if-let [b (first r)]
      (let [diff (- b a)
            next-sign (signum diff)
            bad-levels? (or (not= (compare-level-changes sign next-sign)
                                  :consistent)
                            (> (Math/abs diff) 3))]
        (if bad-levels?
          (if (zero? (count (rest r)))
            (throw (ex-info (str "Unsafe regardless of which level is removed")
                            {}))
            (recur a (rest r) sign))
          (recur b (rest r) next-sign)))
      true)))

(def show-reports (make-reports-viewer safe-report-with-dampener?))

(show-reports ex1-reports)

;; In the real data, we get a much more mixed result than before:
(show-reports (take 40 input-reports))


(defn solve-2
  [input]
  (count (remove nil?
                 (map #(try (safe-report-with-dampener? %)
                            (catch Exception _ nil))
                      (parse-input input)))))

(= (solve-2 ex1) 4)
(solve-2 input) ;; incorrect → 614 too low

