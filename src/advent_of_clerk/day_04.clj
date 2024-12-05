;; # 🎄 Advent of Clerk: Day 4
(ns advent-of-clerk.template
  (:require [nextjournal.clerk :as clerk]
            [advent-of-clerk.utils :as utils]
            [clojure.set :as set]
            [clojure.string :as str]))

(def input (utils/load-input "day_04.txt"))

;; ---
;; ## Part 1

(def ex1
  "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
")



(defn parse-input-1
  [input]
  (str/split-lines input))

(defn make-coords
  "Returns a sequence of all `[y x]` coordinate pairs from a given grid.
  - grid: sequence of sequences of the same length"
  [grid]
  (let [h (count grid)
        w (count (first grid))]
    (for [y (range h)
          x (range w)]
      [y x])))

(defn subs-at-index?
  "Returns true if string `s` contains the given `substr` at index `i`."
  ([substr s i backwards?]
   (let [s (if backwards? (str/reverse s) s)
         i (if backwards? (- (count s) 1 i) i)]
     (subs-at-index? substr s i)))
  ([substr s i]
   (when (>= (- (count s) i) (count substr))
     (= (subs s i (+ i (count substr))) substr))))

(defn get-index-at-diag
  "Returns the index of coord `[y x]` at its diagonal of `type` (either `:main` or `:anti`).
  - requires the width `w` of the grid as input"
  [y x w type]
  (case type
    :main (let [di (- y x)]
            (if (>= di 0) x y))
    :anti (let [di (+ y x)]
            (if (>= di w) (- w 1 x) y))))

(defn transpose-vertical
  [grid]
  (mapv (fn [i] (str/join (map #(get % i) grid)))
        (range (count (first grid)))))

(defn transpose-diagonal
  [grid]
  (let [coords (make-coords grid)]
    [(update-vals
      (group-by (partial apply -) coords)
      (fn [coords] (str/join (map #(get-in grid %) coords))))
     (update-vals
      (group-by (partial apply +) coords)
      (fn [coords] (str/join (map #(get-in grid %) coords))))]))

(defn count-substr-occurrences
  [substr lines]
  (let [w (count (first lines))
        hz-lines lines
        vt-lines (transpose-vertical lines)
        [dg-main-lines
         dg-anti-lines] (transpose-diagonal lines)
        at-index? (partial subs-at-index? substr)]
    (for [[y hz-line] (map-indexed vector hz-lines)
          [x c]       (map-indexed vector hz-line)
          :let [vt-line (vt-lines x)
                dg-main-line (dg-main-lines (- y x))
                dg-anti-line (dg-anti-lines (+ y x))
                i-main (get-index-at-diag y x w :main)
                i-anti (get-index-at-diag y x w :anti)]]
      (when (= c \X)
        (count (filter true?
                       [(at-index? hz-line x)         ;; →
                        (at-index? hz-line x true)    ;; ←
                        (at-index? vt-line y)         ;; ↓
                        (at-index? vt-line y true)    ;; ↑
                        (at-index? dg-main-line i-main)      ;; ↘
                        (at-index? dg-main-line i-main true) ;; ↖
                        (at-index? dg-anti-line i-anti)      ;; ↙
                        (at-index? dg-anti-line i-anti true) ;; ↗
                        ]))))))

(defn solve-1
  [input]
  (->> input
       parse-input-1
       (count-substr-occurrences "XMAS")
       (remove nil?)
       (apply +)))

(= (solve-1 ex1) 18)
(= (solve-1 input) 2378)

;; ### Experiments

(def hz-lines (str/split-lines ex1))

;; Regex can’t match overlapping words:
(re-seq #"XMAS|SAMX" "XMASAMX.MM")

(def XMAS "XMAS")

(def xmas-at-index? (partial subs-at-index? XMAS))

;; Example line:
(def line "XMASAMX.MM")

(str/reverse line)

;; Some tests:
(xmas-at-index? line 0)
(xmas-at-index? line 6)
(xmas-at-index? line 6 true)
(xmas-at-index? line 7)

;; This gets me the horizontal XMAS’es:
(for [i (range (count line))
      :let [c (get line i)]]
  (when (= c \X)
    (count (filter true?
                   [(xmas-at-index? line i)      ;; →
                    (xmas-at-index? line i true) ;; ←
                    ]))))

;; Getting a vertical line is as simple as:
(str/join (map #(get % 0) hz-lines))

;; But doing this for every line is wasteful, so:
(def vt-lines
  (mapv (fn [i] (str/join (map #(get % i) hz-lines)))
        (range (count (first hz-lines)))))

(def xmas-counts
  (for [[y hz-line] (map-indexed vector hz-lines)
        [x c]       (map-indexed vector hz-line)
        :let [vt-line (vt-lines x)]]
    (when (= c \X)
      (count (filter true?
                     [(xmas-at-index? hz-line x)      ;; →
                      (xmas-at-index? hz-line x true) ;; ←
                      (xmas-at-index? vt-line y)      ;; ↓
                      (xmas-at-index? vt-line y true) ;; ↑
                      ])))))

(apply + (remove nil? xmas-counts))

;; This worked:

^{::clerk/visibility {:code :hide}}
(clerk/html
 [:pre {:style {:line-height 0.9}}
  "MMMSXXMASM 1
MSAMXMSMSA 1
AMXSXMAAMM 0
MSAMASMSMX 0
XMASAMXAMM 2
XXAMMXXAMA 0
SMSMSASXSS 0
SAXAMASAAA 0
MAMMMXMMMM 0
MXMXAXMASX 1
 
0000001002"])

;; 3 + 5 = 8

;; Every _x_-index has two crossing diagonals.

;; I could create the diagonals for every _xy_-position, but that would again be wasteful.

;; - The index for the same diagonal line is shifted with every _y_-index.
;; - Some diagonals are only reachable from a certain range of _y_-indexes if _height > length_.


;; Let’s study an example matrix to see if there are any numerical patterns:

(def matrix
  [[0x0 0x1 0x2]
   [0x3 0x4 0x5]
   [0x6 0x7 0x8]
   [0x9 0xa 0xb]
   [0xc 0xd 0xe]])

;; Digits are supposed to be in HEX format, so:

;; `a=10, b=11, c=12, d=13, e=14`

;; These are all the diagonals:

^{::clerk/visibility {:code :hide}}
(clerk/html [:pre {:style {:line-height 0.9}} "
0--  0--  -1-  -1-  --2  --2
-4-  ---  --5  3--  ---  -4-
--8  ---  ---  ---  ---  6--
---  ---  ---  ---  ---  ---
---  ---  ---  ---  ---  ---
                            
---  -1-  0--  --2  -1-  ---
3--  3--  -4-  -4-  --5  --5
-7-  ---  --8  6--  ---  -7-
--b  ---  ---  ---  ---  9--
---  ---  ---  ---  ---  ---
                            
---  --2  ---  ---  0--  ---
---  -4-  3--  --5  -4-  ---
6--  6--  -7-  -7-  --8  --8
-a-  ---  --b  9--  ---  -a-
--e  ---  ---  ---  ---  c--
                            
---  ---  ---  ---  ---  ---
---  --5  ---  ---  3--  ---
---  -7-  6--  --8  -7-  ---
9--  9--  -a-  -a-  --b  --b
-d-  ---  --e  c--  ---  -d-
                            
---  ---  ---  ---  ---  ---
---  ---  ---  ---  ---  ---
---  --8  ---  ---  6--  ---
---  -a-  9--  --b  -a-  ---
c--  c--  -d-  -d-  --e  --e
"])

;; Maybe we can somehow encode each diagonal as a single number that contains all of its _xy_-indexes, so that at any position we can find all diagonals corresponding to it.

;; Let’s look at the actual _xy_-positions:

(def coords (for [y (range 5)
                  x (range 3)]
              [y x]))

(clerk/table (partition 3 coords))

;; It seems like for downward diagonals, the **difference** $x - y$ for any position $[x,y]$ of the diagonal must be the same:

(clerk/table (partition 3 (map (partial apply -) coords)))

;; It makes sense, because if we add the same amount for _x_ and _y_, both numbers will be the same distance apart on the number line.

;; Likewise, for upward diagonals, the **sum** $x + y$ for any position $[x,y]$ of the diagonal must be the same:

(clerk/table (partition 3 (map (partial apply +) coords)))

;; This also makes sense, because if we add the same amount to one number and subtract the same amount from the other, there is no difference in the total amount.

;; Now the question remains if these “diagonal numbers” are unique.

;; In the “difference matrix”:
;; - position _[0,0]_ determines the index 0 downward diagonal
;; - every _y+1_ row of the first column uniquely determines a downward diagonal of index _y+1_
;; - every _x+1_ column of the first row uniquely determines a downward diagonal of index _-(x+1)_
;; - these must be the only diagonals in the matrix

;; In the “sum matrix”:
;; - position _[0,0]_ determines the index 0 upward diagonal
;; - every _y+1_ row of the first column uniquely determines a upward diagonal of index _y+1_
;; - every _x+1_ column of the last row uniquely determines a upward diagonal of index _y+x+1_
;; - these must be the only diagonals in the matrix


;; So it is easy to collect and group all the diagonals by index:

^{::clerk/auto-expand-results? true}
(def dg-main->pos (->> coords
                       (group-by (partial apply -))))

^{::clerk/auto-expand-results? true}
(def dg-anti->pos (->> coords
                       (group-by (partial apply +))))

;; Then we can use those maps to extract the diagonals for each position in the original matrix:

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(def cols (mapv #(str "hsl(" (* (/ % 6.0) 360) ", 100%, 35%)") (range 7)))
^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn ->hex [n] (format "%x" n))

(clerk/table
 (for [[y x] coords
       :let [n (get-in matrix [y x])]]
   {:self (clerk/html (->hex n))
    :main (clerk/html [:code {:style {:color (cols (+ 2 (- y x)))}}
                       (map (comp ->hex (partial get-in matrix))
                            (dg-main->pos (- y x)))])
    :anti (clerk/html [:code {:style {:color (cols (+ y x))}}
                       (map (comp ->hex (partial get-in matrix))
                                  (dg-anti->pos (+ y x)))])}))

;; We see from the color-coding that every group of numbers in `main` or `anti` consists of all corresponding `self` numbers, so the results are consistent.

(def diag->coords
  (let [input hz-lines
        coords (make-coords hz-lines)]
      {:main (update-vals
              (group-by (partial apply -) coords)
              (fn [coords] (str/join (map #(get-in input %) coords))))
       :anti (update-vals
              (group-by (partial apply +) coords)
              (fn [coords] (str/join (map #(get-in input %) coords))))}))



(get-index-at-diag 3 1 3 :anti)
(get-index-at-diag 3 1 3 :main)

(get-index-at-diag 3 2 3 :anti)
(get-index-at-diag 3 2 3 :main)

(get-index-at-diag 0 2 3 :anti)
(get-index-at-diag 0 2 3 :main)

(def counts
  (let [w (count (first hz-lines))]
    (for [[y hz-line] (map-indexed vector hz-lines)
          [x c]       (map-indexed vector hz-line)
          :let [vt-line (vt-lines x)
                dg-main-line ((:main diag->coords) (- y x))
                dg-anti-line ((:anti diag->coords) (+ y x))
                i-main (get-index-at-diag y x w :main)
                i-anti (get-index-at-diag y x w :anti)]]
      (when (= c \X)
        (count (filter true?
                       [(xmas-at-index? hz-line x)         ;; →
                        (xmas-at-index? hz-line x true)    ;; ←
                        (xmas-at-index? vt-line y)         ;; ↓
                        (xmas-at-index? vt-line y true)    ;; ↑
                        (xmas-at-index? dg-main-line i-main) ;; ↘
                        (xmas-at-index? dg-main-line i-main true) ;; ↖
                        (xmas-at-index? dg-anti-line i-anti)      ;; ↙
                        (xmas-at-index? dg-anti-line i-anti true) ;; ↗
                        ]))))))

(apply + (remove nil? counts))

;; ### Observations

;; ---
;; ## Part 2

(def ex2 ex1)
(def parse-input-2 parse-input-1)

(defn count-X-MAS
  [lines]
  (for [[y hz-line] (map-indexed vector lines)
        [x c]       (map-indexed vector hz-line)]
    (when (= c \A)
      (let [c↖ (get-in lines [(- y 1) (- x 1)])
            c↘ (get-in lines [(+ y 1) (+ x 1)])
            c↗ (get-in lines [(- y 1) (+ x 1)])
            c↙ (get-in lines [(+ y 1) (- x 1)])]
        (when (and (or (and (= \M c↖) (= \S c↘))
                       (and (= \S c↖) (= \M c↘)))
                   (or (and (= \M c↗) (= \S c↙))
                       (and (= \S c↗) (= \M c↙))))
          1)))))

(defn solve-2
  [input]
  (->> (parse-input-2 input)
       count-X-MAS
       (remove nil?)
       count))

(= (solve-2 ex1) 9)
(= (solve-2 input) 1796)

;; ### Experiments

;; This time I use a simpler, more specific approach, doing just a lookup around a center coordinate without building strings in all directions and taking substrings:

(def ex-lines (str/split-lines ex2))

(def X-MAS-count
  (let [lines ex-lines]
    (for [[y hz-line] (map-indexed vector lines)
          [x c]       (map-indexed vector hz-line)]
      (when (= c \A)
        (let [c↖ (get-in lines [(- y 1) (- x 1)])
              c↘ (get-in lines [(+ y 1) (+ x 1)])
              c↗ (get-in lines [(- y 1) (+ x 1)])
              c↙ (get-in lines [(+ y 1) (- x 1)])]
          (when (and (or (and (= \M c↖) (= \S c↘))
                         (and (= \S c↖) (= \M c↘)))
                     (or (and (= \M c↗) (= \S c↙))
                         (and (= \S c↗) (= \M c↙))))
            1))))))

(count (remove nil? X-MAS-count))

;; ### Observations


(comment

  )

