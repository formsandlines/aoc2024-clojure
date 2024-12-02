;; # 🎄 Advent of Clerk 2024

;; - by [Peter Hofmann](https://github.com/formsandlines)

;; My attempt at [Advent of Code 2024](https://adventofcode.com) with
;; [Clerk](https://clerk.vision).
(ns advent-of-clerk.index
  {:nextjournal.clerk/visibility {:code :hide :result :hide}}
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [nextjournal.clerk :as clerk]))

#_(days-with-contents)

(defn build-paths
  "Computes the paths to build by looking for files in
  `src/advent_of_clerk` and filtering out unmodified templates (files
  with less than four lines)."
  []
  (let [root   "src"
        subdir "advent_of_clerk"]
    (vec
     (concat
      (into []
            (keep (fn [day]
                    (let [f (fs/file
                             root subdir
                             (format "day_%s.clj"
                                     (cond->> day (< day 10) (str "0"))))]
                      (when (and (.exists f)
                                 (< 3 (count (str/split-lines (slurp f)))))
                        (str f)))))
            (range 25))
      ;; additional notebooks
      [;; (str (fs/file root subdir "day_02_logic.clj"))
       ]))))

#_(build-paths)

{:nextjournal.clerk/visibility {:result :show}}

^::clerk/no-cache
(clerk/html
 (into [:ul]
       (mapv (fn [path]
               (let [day      (second (re-matches #".*day_(\d+).*.clj?" path))
                     appendix (second (re-matches #".*day_\d+_(.*).clj?" path))]
                 (when (some? day)
                   [:li [:a {:href (clerk/doc-url path)}
                         "Day " day
                         (if (some? appendix) (str " / " appendix) "")]])))
             (build-paths))))

