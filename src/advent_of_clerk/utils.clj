(ns advent-of-clerk.utils
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn load-input [filename]
  (slurp (io/resource filename)))
