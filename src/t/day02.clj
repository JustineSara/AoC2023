(ns t.day02
  (:gen-class)
  (:require
   [clojure.string :as str]))

(def inputd2
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(defn iscolorok?
  [color numb cubes]
  (reduce (fn [a b] (and a b)) true
          (map (fn [b] (<= (parse-long (re-find #"\d+" b)) numb))
               (re-seq (re-pattern (str "\\d+ " color)) cubes))))
;; reduce needs a function of two elements
;; `and` is _not_ a function so we have to define one
;; we set `true` as a starting point in case the sequence has only one element

(defn d2part1
  [input]
  (apply +
         (map (fn [[g c]] (if
                           (and
                            (iscolorok? "red" 12 c)
                            (iscolorok? "green" 13 c)
                            (iscolorok? "blue" 14 c))
                            (parse-long (re-find #"\d+" g))
                            0))
              (map (fn [l] (str/split l #":" 2)) (str/split-lines input)))))
;; use destructuring 
;; fn [input]
;; input is [game color]
;; so : fn[[game color]]


(defn mincolor
  [color cubes]
  (reduce (fn [a b] (if (> a b) a b)) 0
          (map (fn [b] (parse-long (re-find #"\d+" b)))
               (re-seq (re-pattern (str "\\d+ " color)) cubes))))
;; note : max is a function that exists ^^

(defn d2part2
  [input]
  (apply +
         (map (fn [ll] (* (mincolor "red" (last ll))
                          (mincolor "green" (last ll))
                          (mincolor "blue" (last ll))))
              (map (fn [l] (str/split l #":" 2)) (str/split-lines input)))))

(def cubegame4
  "1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red")

(defn mainD2
  []
  (println "Day 2 - cube game")
  (println "~~~ part1 ~~~")
  (println (d2part1 inputd2))
  ;; (println (iscolorok? "blue" 14 "1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"))
  (println (d2part1 (slurp "input/day2.txt")))
  (println "~~~ part2 ~~~")
  ;; (println "Game 4: " cubegame4)
  ;; (println "blue : " (mincolor "blue" cubegame4))
  ;; (println "green : " (mincolor "green" cubegame4))
  ;; (println "Game 4 power : " (* (mincolor "blue" cubegame4) (mincolor "green" cubegame4) (mincolor "red" cubegame4)))
  (println (d2part2 inputd2))
  (println (d2part2 (slurp "input/day2.txt"))))