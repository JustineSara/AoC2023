(ns t.core
  (:gen-class)
  (:require
   [clojure.string :as str]
   [clojure.set :as cljset]))

(def input "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")


(def input2
  "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(defn sol1
  [input]
  (apply +
         (map
          (fn [l] (Integer. (str (re-find #"\d" l) (re-find #"\d" (str/reverse l)))))
          (str/split-lines input))))


(defn maptonumber
  [t]
  ({"1" 1 "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9
    "one" 1 "two" 2 "three" 3 "four" 4 "five" 5 "six" 6 "seven" 7 "eight" 8 "nine" 9
    "eno" 1 "owt" 2 "eerht" 3 "ruof" 4 "evif" 5 "xis" 6 "neves" 7 "thgie" 8 "enin" 9} t))

(defn sol2
  [input]
  (apply +
         (map
          (fn [l] (parse-long (str
                               (maptonumber (re-find #"\d|one|two|three|four|five|six|seven|eight|nine" l))
                               (maptonumber (re-find #"\d|eno|owt|eerht|ruof|evif|xis|neves|thgie|enin" (str/reverse l))))))
          (str/split-lines input))))

;; Integer. : bad idea
;;    capital letter = java class 
;;    CapitalLetter + "." = contructor of the java class
;;    - creates new object
;;    - Integer : entier en 32bits  ==> max is 2.10^9 : not that much with advent of code and no warning/error
;; 
;; better solve : integer 64bit
;; Long (don't use "Long." because again new object)
;;    parse-long


(defn mainD1
  []
  (println "Hello, World!")
  (println "Part 1")
  (println input)
  (println (sol1 input))
  (println "With my input:")
  (println (sol1 myinput))
  (println "\nPart 2")
  (println input2)
  (println (sol2 input2))
  (println "With my input:")
  (println (sol2 myinput))
  (println (sol2 (slurp "input/day1.txt"))))

  ;; (println "2two1sevenine")
  ;; (println (re-find #"\d|one|two|three|four|five|six|seven|eight|nine" "2two1sevenine"))
  ;; (println (map maptonumber 
  ;;               [(re-find #"\d|one|two|three|four|five|six|seven|eight|nine" "2two1sevenine") (re-find #"\d|eno}owt|eerht|ruof|evif|xis|neves|thgie|enin" (str/reverse "2two1sevenine"))]))


;; 55427 too high

  ;; (println "pqr3stu8vwx")
  ;; (println ((fn [l] (Integer. (str (re-find #"\d" l) (re-find #"\d" (str/reverse l))))) "pqr3stu8vwx"))
  ;; (println (slurp ))


