(ns t.d24
  (:gen-class)
  (:require
   [clojure.string :as cljstr]
   [clojure.set :as cljset]))

(def sample "19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3
")

(defn parse-input
  [input]
  (->> input
       cljstr/split-lines
       (map (fn [l] (cljstr/split l #" *[@|,] *")))
       (map (fn [ns] (map parse-long ns)))
       ))


(defn meets?
  "Check if hailstorms (lines) meets = if the two lines are not //
     The oterh case is if the time is negative (they go away from each other) but we will not considere it here"
  [[x1 y1 _ vx1 vy1 _] [x2 y2 _ vx2 vy2 _]]
   (not= (* vx1 vy2) (* vx2 vy1) )
  )

#_(defn check-all-pair
  [hailstorms]
  
  (for [i1 (range (dec (count hailstorms)))
        i2 (range (inc i1) (count hailstorms))]
    (let [[x1 y1 _ vx1 vy1 _ :as h1] (nth hailstorms i1)
          [x2 y2 _ vx2 vy2 _ :as h2] (nth hailstorms i2)]
      (newline)
      (println "h1 ("i1")" h1)
      (println "h2 ("i2")" h2)
      (if (meets? h1 h2)
        (let [t2 (/ (- (* (- y1 y2) vx1) (* (- x1 x2) vy1)) (- (* vx1 vy2) (* vx2 vy1)))
              t1 (/ (- (* (- y2 y1) vx2) (* (- x2 x1) vy2)) (- (* vx2 vy1) (* vx1 vy2)))
              x (+ x2 (* t2 vx2) )
              y (+ y2 (* t2 vy2))]
          (if (and (>= t2 0) (>= t1 0))
            (println "  meet at " (double x) (double y))
            (println "  meet in the non-existent past : " t1 t2)
            )
          )
        (println "  does not meet : parallele lines")
        ))))


(defn check-all-pair-loops
  [hailstorms minX maxX minY maxY]

  (loop [[x1 y1 _ vx1 vy1 _ :as h1] (first hailstorms)
         hailstorms1 (rest hailstorms)
         hailstorms2 (rest hailstorms)
         Nmeet 0]

    (if (empty? hailstorms2)
      (if (= 1 (count hailstorms1))
        Nmeet
        (recur (first hailstorms1)
               (rest hailstorms1)
               (rest hailstorms1)
               Nmeet))
      (let [[[x2 y2 _ vx2 vy2 _ :as h2] & hailstorms2] hailstorms2]
        ;; (newline)
        ;; (println "h1 " h1)
        ;; (println "h2 " h2)
        (if (meets? h1 h2)
          (let [t2 (/ (- (* (- y1 y2) vx1) (* (- x1 x2) vy1)) (- (* vx1 vy2) (* vx2 vy1)))
                t1 (/ (- (* (- y2 y1) vx2) (* (- x2 x1) vy2)) (- (* vx2 vy1) (* vx1 vy2)))
                x (+ x2 (* t2 vx2))
                y (+ y2 (* t2 vy2))]
            (if (and (>= t2 0) (>= t1 0) (<= minX x maxX) (<= minY y maxY))
              (recur h1 hailstorms1 hailstorms2 (inc Nmeet))  ;; (println "  meet at " (double x) (double y))
              (recur h1 hailstorms1 hailstorms2 Nmeet)        ;; (println "  meet in the non-existent past : " t1 t2)))  ;; or outside of box
              ))
          (recur h1 hailstorms1 hailstorms2 Nmeet) )          ;; (println "  does not meet : parallele lines")
    ))))


(defn d24p1
  [input pmin pmax]
  (let [hailstorms (parse-input input)
        ]
     (check-all-pair-loops hailstorms pmin pmax pmin pmax)
  ))

(defn d24p2
  [input]
  (let [x (parse-input input)
        ]
    x
  ))

(defn -main
  [& args] 
  (println "day24")
  (println sample)
  (newline)
  
  (println "part1")
  (prn (d24p1 sample 7 27))
 (prn (d24p1 (slurp "input/day24.txt") 200000000000000 400000000000000))
  
;;  (newline)
;;  (println "part2")
;;  (prn (d24p2 sample))
;;  (prn (d24p2 (slurp "input/day24.txt")))
  )
