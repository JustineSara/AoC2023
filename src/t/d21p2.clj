(ns t.d21p2
  (:gen-class)
  (:require
   [clojure.string :as cljstr]
   [clojure.set :as cljset]))

(def sample "...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........")

(defn parse-input
  [input]
  (->>
   (cljstr/split-lines input)
   (map-indexed (fn [y l] (map-indexed (fn [x c] [x y c]) l)))
   (apply concat)
   (filter (fn [[_ _ c]] (not= c \#)))
   ((fn [all]
      [(->>
        all
        (filter (fn [[_ _ c]] (= c \S)))
        first
        ((fn [[x y _]] [x y])))
       (->>
        all
        (map (fn [[x y _]] [x y]))
        set)
       [(apply min (map first all)) (apply max (map first all))]
       [(apply min (map second all)) (apply max (map second all))]
       ]))))


(defn garden-num
  [[gx gy] [x y] minX maxX minY maxY]
  (cond
    (= x (dec minX)) [[(dec gx) gy] [maxX y]]
    (= y (dec minY)) [[gx (dec gy)] [x maxY]]
    (= x (inc maxX)) [[(inc gx) gy] [minX y]]
    (= y (inc maxY)) [[gx (inc gy)] [x minY]]
    :else [[gx gy] [x y]]
    )
  )

(defn one-step
  [all-pos-s gardens [minX maxX minY maxY]]
  (->>
   (for [[gard [x y]] all-pos-s]
     (->> [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]
          (map (fn [p] (garden-num gard p minX maxX minY maxY)))
          (filter (fn [[_ [x y]]] (contains? gardens [x y])))
          set
        )
     )
   (apply cljset/union)   
   ))


(defn filter-border
  [l-points]
;;   list of points, point is [step x y]
;;   (prn l-points)
  (loop [l-pts l-points
         saved-pts []]
    (if (empty? l-pts) 
      (let [minT (apply min (map first saved-pts))]
        (into {} (map (fn [[t x y]] [(- t minT) [x y]]) saved-pts))
        )
        (let [[[t x y :as pt] & l-pts] l-pts
              l-pts-neighbors (->> l-pts
                                   (filter (fn [[tt _  _]] (= (dec t) tt)))
                                   (filter (fn [[_  xx yy]] (= 1 (+ (abs (- xx x)) (abs (- yy y)))))))]
          (if (empty? l-pts-neighbors)
            (recur l-pts (conj saved-pts pt))
            (recur l-pts saved-pts))))))

(defn walk-one-map
  [pos-s gardens minmax]
  (loop [
         p-s [pos-s]
         pos-viewed #{}
         pos-out {}
         nstep 0
         Nvisited [1]
         [Neven Nodd] [1 0]
         ]
    ;; (println "  " nstep " : " p-s)
    ;; (prn pos-viewed)
    (if (empty? p-s)
      [Neven Nodd nstep (into {} (map (fn [[k v]] [k (filter-border v) ]) pos-out)) Nvisited]
      (let [new-p-s (one-step (map (fn [p] [[0 0] p]) p-s) gardens minmax)
            p-s-out (->> new-p-s
                     (filter (fn [[g _]] (not= g [0 0])) )  
                     (reduce (fn [po [g [px py]]] po (assoc po g (conj (get po g) [(inc nstep) px py]))) pos-out ))
            p-s-in (->> new-p-s
                    (filter (fn [[g _]] (= g [0 0])))
                    (filter (fn [[_ p]] (not (contains? pos-viewed p))))
                        (map second)
                        )]
        ;; (println nstep Nvisited "  " p-s-in)
        ;; (println "  " p-s-out)
        (recur 
          p-s-in 
          (set p-s)
          p-s-out 
          (inc nstep)
          (concat Nvisited [(count p-s-in)])
          (if (zero? (mod nstep 2)) [Neven (+ Nodd (count p-s-in))] [ (+ Neven (count p-s-in)) Nodd]) )
        ))))

(def walk-one-map-memo (memoize walk-one-map))

(defn walk-maps
  [maps-tbw Nsteps gardens minmax]
;; maps-tbw is a list of "maps" : { map-indices [step-on-which-enters  pos-on-which-enter]}
  (loop [m-tbw maps-tbw
         n-m-tbw {}
         [Neven Nodd] [0 0]]
    (if (empty? m-tbw)
      [n-m-tbw Neven Nodd]
      (let [[[[MX MY] [NstepsStart pos0]] & m-tbw] m-tbw
            [neven nodd nstep pos-out Nvisited] (walk-one-map-memo pos0 gardens minmax)
            n-m-tbw (->> pos-out
                         (map (fn [[[mx my] [steps p]]] [[(+ MX mx ) (+ MY my)] [steps p]]))
                         (map (fn [[m [steps p]]] (when (and (< 1 steps) (<= (+ steps NstepsStart) Nsteps) (not (contains? n-m-tbw m))) [m [(+ steps NstepsStart) p]])))
                         (keep identity)
                         (reduce (fn [n-m [k v]] (assoc n-m k v)) n-m-tbw ))
            ;; _ (println " " NstepsStart pos0)
            ;; _ (println "   " nstep neven nodd pos-out)
            ;; _ (println "      " n-m-tbw)
            ]
        (if (< Nsteps (+ NstepsStart nstep))
          (let [neven (apply + (map (fn [n s] (if (even? s) n 0) ) Nvisited (range (inc (- Nsteps NstepsStart)))))
                nodd (apply + (map (fn [n s] (if (odd? s) n 0)) Nvisited (range (inc (- Nsteps NstepsStart)))))]
            ;; (println "*** need to stop inside the map")
            ;; (println "***" Nvisited)
            ;; (println "***" (range (inc (- Nsteps NstepsStart))))
            (recur m-tbw n-m-tbw (if (even? NstepsStart) [(+ Neven neven) (+ Nodd nodd)] [(+ Neven nodd) (+ Nodd neven)]))
          )
          (recur m-tbw n-m-tbw (if (even? NstepsStart) [(+ Neven neven) (+ Nodd nodd)] [(+ Neven nodd) (+ Nodd neven)]))
          )
        )
  )))

(defn walk-all-maps
  [maps-tbw Nsteps gardens minmax]
  (loop [m-tbw maps-tbw
         Neven 0
         Nodd 0]
    (let [[new-m-tbw neven nodd] (walk-maps m-tbw Nsteps gardens minmax)]
    ;;   (println "~~~~" new-m-tbw neven nodd)
      (if (empty? new-m-tbw) 
        [(+ Neven neven) (+ Nodd nodd)]
        (recur new-m-tbw (+ Neven neven) (+ Nodd nodd))))
  ))

(defn d21
  [input Nsteps]
  (let [[pos0 gardens [minX maxX] [minY maxY]] (parse-input input)
        minmax [minX maxX minY maxY]
        ;; [Neven Nodd] (walk-all-maps {[0 0] [0 pos0]} Nsteps gardens minmax)
        ]
    (println "start at " pos0)
    (println "maps from " minX minY " to " maxX maxY)
    ;; (one-step [[[0 0] [0 0]]] gardens [minX maxX minY maxY])
    ;; (walk-one-map pos0 gardens [minX maxX minY maxY])
    ;; (walk-one-map [10 4] gardens minmax)
    (let [[Neven Nodd nstep pos-out Nvisited] (walk-one-map pos0 gardens minmax)
          minS (into {} (map (fn [[k v]] [k (apply min (map first v))]) pos-out))]
      
      (->> pos-out
        ;;    first
        ;;    second
        ;;    (group-by first)
        ;;    (map (fn [[k v]] [k (map second v)]))
        ;;    (into {})
        ;;    (sort-by first >)
           )

      )
    ;; (if (even? Nsteps) Neven Nodd)
  ))



(defn -main
  [& args]
  (println "day21")
  (println sample)
  (newline)

  (println "part2")
  (newline)
  (println "6 steps => Expect 16")
  (prn (d21 sample 6))
  (newline)
;;   (println "10 steps => Expect 50")
;;   (prn (d21 sample 10))
;;   (newline)
;;   (println "50 steps => Expect 1594")
;;   (prn (d21 sample 50))
;;   (newline)
;;  (prn (d21 (slurp "input/day21.txt") 64))
;;  (prn (d21 (slurp "input/day21.txt") 26501365))

  )

;; 6107040063 is too low
;; 