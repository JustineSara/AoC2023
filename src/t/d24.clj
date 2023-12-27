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


(defn test-velocity
  ;; test not parallele
  [[_ _ _ vx vy vz] [VX VY VZ]]
  (or
   (not= (* vx VY) (* VX vy))
   (not= (* vx VZ) (* VX vz))
   (not= (* vz VY) (* VZ vy))
   )
  )

(defn isparallele?
  ;; test if parallele
  [[_ _ _ vx vy vz] [_ _ _ VX VY VZ]]
  (and
   (= (abs (* vx VY)) (abs (* VX vy)))
   (= (abs (* vx VZ)) (abs (* VX vz)))
   (= (abs (* vz VY)) (abs (* VZ vy)))))

(defn range-V
  [hailstorms]
  (loop [ranges [[[##-Inf ##Inf] [##-Inf ##Inf]]]
         hst hailstorms]
    (if (empty? hst)
      ranges
      (let [[[x _ _ vx _ _ :as h] & hst] hst
            ;; _ (println ranges)
            ;; _ (println x vx)
            new-ranges
            (->> ranges
                 (mapcat (fn [[[xmin xmax] [vmin vmax]]]
                           (cond
                             (<= x xmin) [[[xmin xmax] [vmin (min vmax vx)]]]
                             (<= xmin x xmax) [[[xmin x] [(max vx vmin) vmax]] [[x xmax] [vmin (min vmax vx)]]]
                             (<= xmax x) [[[xmin xmax] [(max vmin vx) vmax]]])))
                 (filter (fn [[[xmin xmax] [vmin vmax]]] (<= vmin vmax))))
            ;; _ (prn new-ranges)
            ]
        (recur new-ranges hst)))
    ))


(defn meet-2d
  [[x1 y1 z1 vx1 vy1 vz1 :as h1] [x2 y2 z2 vx2 vy2 vz2 :as st]]
  (let [t2 (/ (- (* (- y1 y2) vx1) (* (- x1 x2) vy1)) (- (* vx1 vy2) (* vx2 vy1)))
        t1 (/ (- (* (- y2 y1) vx2) (* (- x2 x1) vy2)) (- (* vx2 vy1) (* vx1 vy2)))
        ;; x (+ x2 (* t2 vx2))
        ;; y (+ y2 (* t2 vy2))
        zc1 (+ z1 (* t1 vz1))
        zc2 (+ z2 (* t2 vz2))]
    (cond
      (and (= t1 t2) (>= t1 0) (int? t1) (= zc1 zc2) (int? zc1)) ["is good!" 0 0]
      (and (< t2 0) (< t1 0)) ["both in past" (float (- t2 t1)) (float (- zc2 zc1))]
      (< t2 0) ["only stone in the past" (float (- t2 t1)) (float (- zc2 zc1))]
      (< t1 0) ["only storm in the past" (float (- t2 t1)) (float (- zc2 zc1))]
      ;; (not= t1 t2) "times "
      (> zc1 zc2) ["z-h > z-stone" (float (- t2 t1)) (float (- zc2 zc1))]
      (< zc1 zc2) ["z-h < z-stone" (float (- t2 t1)) (float (- zc2 zc1))]
      :else ["else" (float (- t2 t1)) (float (- zc2 zc1))]
      )
    )
  )

(defn testmeet3d
  [h1 st]
  (if
    (isparallele? h1 st) "parallele"
    (meet-2d h1 st)
    )
  )

(defn meet-2d-num
  [[x1 y1 z1 vx1 vy1 vz1 :as h1] [x2 y2 z2 vx2 vy2 vz2 :as st]]
  (if
   (isparallele? h1 st) [##Inf ##Inf]
   (let [t2 (/ (- (* (- y1 y2) vx1) (* (- x1 x2) vy1)) (- (* vx1 vy2) (* vx2 vy1)))
         t1 (/ (- (* (- y2 y1) vx2) (* (- x2 x1) vy2)) (- (* vx2 vy1) (* vx1 vy2)))
         zc1 (+ z1 (* t1 vz1))
         zc2 (+ z2 (* t2 vz2))]
     [ (abs (- t2 t1)) (abs (- zc2 zc1))])))

(defn stone-from-2-storms
  [[x1 y1 z1 vx1 vy1 vz1 :as h1] [x2 y2 z2 vx2 vy2 vz2 :as h2] t1 t2]
  ;; (prn t1 t2 (- t1 t2))
  (let [VX (/ (- (+ x1 (* t1 vx1)) (+ x2 (* t2 vx2))) (- t1 t2))
        VY (/ (- (+ y1 (* t1 vy1)) (+ y2 (* t2 vy2))) (- t1 t2))
        VZ (/ (- (+ z1 (* t1 vz1)) (+ z2 (* t2 vz2))) (- t1 t2))
        X (+ x1 (* t1 (- vx1 VX)))
        Y (+ y1 (* t1 (- vy1 VY)))
        Z (+ z1 (* t1 (- vz1 VZ)))]
    [X Y Z VX VY VZ]
    )
  )

(defn getdiffs
  [st hailstorms]
  (->> hailstorms
       (mapcat (fn [h] (meet-2d-num h st)))
       )
     )

(defn one-step
  ([h1 h2 t1 t2 alpha hailstorms]
  (let [diff00 (getdiffs (stone-from-2-storms h1 h2 t1 t2) hailstorms)
        ;; diff0+ (getdiffs (stone-from-2-storms h1 h2 t1 (inc t2)) hailstorms)
        ;; diff0- (getdiffs (stone-from-2-storms h1 h2 t1 (dec t2)) hailstorms)
        ;; diff+0 (getdiffs (stone-from-2-storms h1 h2 (inc t1) t2) hailstorms)
        ;; diff-0 (getdiffs (stone-from-2-storms h1 h2 (dec t1) t2) hailstorms)
        ;; [c0+ c0- c-0 c+0] (->> [diff0+ diff0- diff-0 diff+0]
        ;;                        (map (fn [diff] (apply +
        ;;                                               (map (fn [v v0] (if (< v v0) 1 0)) diff diff00)))))
        ]
  (loop [cur-good 0
         cur-dir [0 0]
         dir-tbtested [[0 1] [0 -1] [-1 0] [+1 0]]]
    (if (empty? dir-tbtested)
      cur-dir
      (let [[[d1 d2 :as d] & dir-tbtested] dir-tbtested
            [d1 d2 :as d] [(* alpha d1) (* alpha d2)]
            [d1 d2 :as d] (if (= (+ t1 d1) (+ t2 d2)) [(* 2 d1) (* 2 d2)] d)
            diff (getdiffs (stone-from-2-storms h1 h2 (+ d1 t1) (+ d2 t2)) hailstorms)
            cgooddir (apply + (map (fn [v v0] (if (< v v0) 1 0)) diff diff00))
            ]
        (if (<= cur-good cgooddir) (recur cgooddir d dir-tbtested)
            (recur cur-good cur-dir dir-tbtested))
        ))
    )))
  (
  [h1 h2 t1 t2 hailstorms]
   (one-step h1 h2 t1 t2 1 hailstorms))

  )

(defn d24p2
  [input t1 t2]
  (let [hailstorms (parse-input input)
        ]
    #_(for [VX (range -5 6)
          VY (range -5 6)
          VZ (range -5 6)
          ]
      (println VX VY VZ (every? identity (map #(test-velocity % [VX VY VZ]) hailstorms)))
      
      )
    #_(range-V hailstorms)

    (let [h1 (nth hailstorms 0)
          h2 (nth hailstorms 1)
          h3 (nth hailstorms 2)
          ;; st (stone-from-2-storms h1 h2 7 6)    ;; good times = 5 3
          hs (rest (rest hailstorms))]
      ;; (testmeet3d h3 st)
      ;; (getdiffs st (rest (rest hailstorms)))
      (loop [t1 t1
             t2 t2
             alpha (quot (min t1 t2) 2)
             [pdx pdy] [0 0]
             iter 0]
        (when (zero? (mod iter 10)) (println iter " alpha: "alpha "  times :" t1 "-" t2))
        (let [st (stone-from-2-storms h1 h2 t1 t2)]
          (if (= [0 0] (meet-2d-num h3 st))
            st
            (let [
                  
                  ;; [dir1 dir2] (one-step h1 h2 t1 t2 alpha hs)
                  ;; [dx dy] [dir1 dir2]
                  
                  [dx dy] (one-step h1 h2 t1 t2 hs)
                  [dir1 dir2] [(* alpha dx) (* alpha dy)]

                  alpha (cond (= alpha 1) 1
                              (= [pdx pdy] [dx dy]) alpha
                              :else (quot (* 9 alpha) 10))                  
                  ;; _ (println "  " dir1 dir2)
                  ]
              (cond
                (= dir1 dir2 0) (recur (+ t1 (rand-int (* 10 alpha))) (+ t2 (rand-int (* 10 alpha))) alpha [dx dy] (inc iter))
                (= (+ t1 dir1) (+ t2 dir2)) (recur (+ t1 (* 2 dir1)) (+ t2 (* 2 dir2)) alpha [dx dy] (inc iter))
                :else (recur (+ t1 dir1) (+ t2 dir2) alpha [dx dy] (inc iter))
                )
              )
            )
          
          )
        )

  )))

(defn d24p2-explore
  [input t1 t2]
  (let [hailstorms (parse-input input)
        h1 (nth hailstorms 0)
        h2 (nth hailstorms 1)
        h3 (nth hailstorms 2)
        hs (rest (rest hailstorms))]
    (loop [t1 t1
           t2 t2
           iter 0]
      (println iter "  times :" t1 "-" t2)
      (let [st (stone-from-2-storms h1 h2 t1 t2)]
        (if (= [0 0] (meet-2d-num h3 st))
          st
          (let [
                [dir1 dir2] (one-step h1 h2 t1 t2 hs)
                ]
            (cond
              (= dir1 dir2 0) (println "no good directions")
              (= dir1 1) (recur (* t1 10) t2  (inc iter))
              (= dir1 -1) (recur (quot t1 10) t2  (inc iter))
              (= dir2 1) (recur t1 (* t2 10) (inc iter))
              (= dir2 -1) (recur t1 (quot t1 10) (inc iter))
              
              )))))))


(defn -main
  [& args] 
  (println "day24")
  (println sample)
  (newline)
  
  (println "part1")
;;   (prn (d24p1 sample 7 27))
;;  (prn (d24p1 (slurp "input/day24.txt") 200000000000000 400000000000000))
  
 (newline)
 (println "part2")
 (prn (d24p2 sample 10 5))
  ;; (prn (d24p2-explore (slurp "input/day24.txt") 10 20))
 (prn (d24p2 (slurp "input/day24.txt") 1000000000000 200000000000))
  )
;; 10000 - 3575500
;; 10000000 - 6 095 500 000
;; 3100  alpha:  450000000000   times : 1000000000000 - 1397050000000000
;; 
;; 0  alpha:  50   times : 100 - 1397050000000000
;; 700  alpha:  45   times : 31605 - 1397050000000000
;;  increases :       1000000
;;                  100000000
;;  doesnot   : 1000000000000
