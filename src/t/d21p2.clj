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
         saved-pts {}]
    (if (empty? l-pts) 
      (let [minT (apply min (map first saved-pts))]
        [minT (into {} (map (fn [[t v]] [(- t minT) v]) saved-pts))]
        ;; saved-pts
        )
        (let [[[t x y :as pt] & l-pts] l-pts
              l-pts-neighbors (->> l-pts
                                   (filter (fn [[tt _  _]] (= (dec t) tt)))
                                   (filter (fn [[_  xx yy]] (= 1 (+ (abs (- xx x)) (abs (- yy y)))))))]
          (if (empty? l-pts-neighbors)
            (recur l-pts (assoc saved-pts t (concat (get saved-pts t []) [[x y]])))
            (recur l-pts saved-pts))))))

(defn walk-one-map
  [pos-in gardens minmax]
;; pos-in is a dictionary of  time : tiles
  (let [pos0s (set (get pos-in 0))
        Npos0 (count pos0s)]
  (loop [
         p-s pos0s
         pos-viewed #{}
         pos-out {}
         nstep 0
         Nvisited [Npos0]
         [Neven Nodd] [Npos0 0]
         ]
    ;; (println "  " nstep " : " p-s)
    ;; (prn pos-viewed)
    (if (empty? p-s)
      [Neven Nodd nstep (into {} (map (fn [[k v]] [k (filter-border v) ]) pos-out)) Nvisited]
      (let [new-p-s (one-step (map (fn [p] [[0 0] p]) p-s) gardens minmax)
            p-s-out (->> new-p-s
                     (filter (fn [[g _]] (not= g [0 0])) )  
                     (reduce (fn [po [g [px py]]] (assoc po g (conj (get po g) [(inc nstep) px py]))) pos-out ))
            p-s-in (->> new-p-s
                    (filter (fn [[g _]] (= g [0 0])))
                    (filter (fn [[_ p]] (not (contains? pos-viewed p))))
                        (map second)
                        (concat (get pos-in (inc nstep)))
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
        )))))

(def walk-one-map-memo (memoize walk-one-map))

(defn walk-maps
  [maps-tbw Nsteps gardens minmax]
;; maps-tbw is a list of "maps" : { map-indices [step-on-which-enters  {i-enter (start at 0) : positions-on-which-enter}]}
  (loop [m-tbw maps-tbw
         n-m-tbw {}
         [Neven Nodd] [0 0]]
    (if (empty? m-tbw)
      [n-m-tbw Neven Nodd]
      (let [[[[MX MY] [NstepsStart pos-in]] & m-tbw] m-tbw
            [neven nodd nstep pos-out Nvisited] (walk-one-map-memo pos-in gardens minmax)
            ;; _ (println "  " MX MY neven nodd)
            n-m-tbw (->> pos-out
                         (map (fn [[[mx my] [steps p]]] [[(+ MX mx ) (+ MY my)] [steps p]]))
                         (map (fn [[m [steps p]]] (when (and (< 1 steps) (<= (+ steps NstepsStart) Nsteps) ) [m 
                                                                                                              (if (contains? n-m-tbw m)
                                                                                                                (let [this-step (+ steps NstepsStart)
                                                                                                                      [prev-step prev-in] (get n-m-tbw m)
                                                                                                                      prev-in (map (fn [[t v]] [(+ t prev-step) v]) prev-in)
                                                                                                                      this-in (map (fn [[t v]] [(+ t this-step) v]) p)
                                                                                                                      new-step (min this-step prev-step)]
                                                                                                                  [new-step
                                                                                                                   (->> prev-in
                                                                                                                        (concat this-in)
                                                                                                                        (group-by first)
                                                                                                                        (map (fn [[t v]] [(- t new-step) (mapcat second v)]))
                                                                                                                        (into {}))])
                                                                                                                [(+ steps NstepsStart) p])])))
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
         Nodd 0
         iter 0]
    ;; (println "~" iter "~") ;; Neven Nodd)
    (when (zero? (mod iter 1000)) (println "~" iter "~")) ;; Neven Nodd)
    (let [[new-m-tbw neven nodd] (walk-maps m-tbw Nsteps gardens minmax)]
    ;;   (println "~~~~~" new-m-tbw neven nodd)
      (if (empty? new-m-tbw) 
        [(+ Neven neven) (+ Nodd nodd)]
        (recur new-m-tbw (+ Neven neven) (+ Nodd nodd) (inc iter))))
  ))


(defn explore-simple-dir
  [Map DIR Nsteps gardens minmax]
  (loop [MAP Map
         dir-sol 0
         dir-start 0
         iter 0]
    (print iter " ")
  (let [[neven nodd nstep map-out Nvisited] (walk-one-map-memo (second MAP) gardens minmax)
        North-start (+ dir-start (first MAP))
        sol_North (if (<= (+ North-start nstep) Nsteps)
                    (if (even? North-start) neven nodd)
                    (if (even? North-start)
                      (apply + (map-indexed (fn [i n] (if (and (even? i) (<= i (- Nsteps North-start))) n 0)) Nvisited))
                      (apply + (map-indexed (fn [i n] (if (and (odd? i) (<= i (- Nsteps North-start))) n 0)) Nvisited))))
        sol_North (+ sol_North dir-sol)
        n1 (get map-out DIR)
          ;;   ne (get map-out [+1 0])
          ;;   nw (get map-out [-1 0])
        [neven nodd nstep map-out Nvisited] (walk-one-map-memo (second n1) gardens minmax)
        n2 (get map-out DIR)]
  
    (if (< Nsteps (+ North-start (first n1)))    ;; only North in it
      sol_North
      (if (not= (second n1) (second n2))
        (recur n1 sol_North North-start (inc iter))
        (let [
            ;;   _ (println " " MAP)
            ;;   _ (println " " n1)
            ;;   _ (println " " n2)
              steps-before-next-map (first n2)
              Ngridstartsin (quot (- Nsteps North-start (first n1)) steps-before-next-map)
            ;;   _ (println "   " steps-before-next-map)
            ;;   _ (println "   " (- Nsteps North-start (first n1)))
            ;;   _ (println "   " Ngridstartsin)
              ]
          (if (zero? Ngridstartsin)                ;; North and n1 are part of it
            (+ sol_North
               (if (even? (first n1))
                 (apply + (map-indexed (fn [i n] (if (and (even? i) (<= i (- Nsteps (first n1)))) n 0)) Nvisited))
                 (apply + (map-indexed (fn [i n] (if (and (odd? i) (<= i (- Nsteps (first n1)))) n 0)) Nvisited))))
            (let [
                ;;   _ (prn  DIR Ngridstartsin  steps-before-next-map  North-start n1)
                  before-last-start (+ (* (dec Ngridstartsin) steps-before-next-map) North-start (first n1))
                  last-start (+ (* Ngridstartsin steps-before-next-map) North-start (first n1))]
                  ;; if before-last is fully in (or not)
              (if (<= (+ before-last-start nstep) Nsteps)
                (let [N-grid-fully-in Ngridstartsin
                      N-like-n2 (quot N-grid-fully-in 2)
                      N-like-n1 (- N-grid-fully-in N-like-n2)]
                  (+ sol_North
                     (* (if (even? (first n1)) neven nodd)  N-like-n1)
                     (* (if (even? (first n2)) neven nodd)  N-like-n2)
                     (if (even? last-start)
                       (apply + (map-indexed (fn [i n] (if (and (even? i) (<= i (- Nsteps last-start))) n 0)) Nvisited))
                       (apply + (map-indexed (fn [i n] (if (and (odd?  i) (<= i (- Nsteps last-start))) n 0)) Nvisited)))))
                (let [N-grid-fully-in (dec Ngridstartsin)
                      N-like-n2 (quot N-grid-fully-in 2)
                      N-like-n1 (- N-grid-fully-in N-like-n2)]
                  (+ sol_North
                     (* (if (even? (first n1)) neven nodd)  N-like-n1)
                     (* (if (even? (first n2)) neven nodd)  N-like-n2)
                     (if (even? before-last-start)
                       (apply + (map-indexed (fn [i n] (if (and (even? i) (<= i (- Nsteps before-last-start))) n 0)) Nvisited))
                       (apply + (map-indexed (fn [i n] (if (and (odd?  i) (<= i (- Nsteps before-last-start))) n 0)) Nvisited)))
                     (if (even? last-start)
                       (apply + (map-indexed (fn [i n] (if (and (even? i) (<= i (- Nsteps last-start))) n 0)) Nvisited))
                       (apply + (map-indexed (fn [i n] (if (and (odd?  i) (<= i (- Nsteps last-start))) n 0)) Nvisited))))))))))))
  ))

(defn walk-with-pattern
  [pos0 Nsteps gardens minmax]
  (let [[Neven Nodd nstep map-out Nvisited] (walk-one-map-memo {0 [pos0]} gardens minmax)
        Ntot Neven
        North (get map-out [0 -1])
        South (get map-out [0 +1])
        East  (get map-out [+1  0])
        West  (get map-out [-1  0])]

    (println Ntot)
    (println North)

    (for [[Map DIR] [[North [0 -1]] [South [0 +1]] [East [+1 0]] [West [-1 0]]]]
      (explore-simple-dir Map DIR Nsteps gardens minmax)
    )


      #_(loop [maps {:N [North]} ;; :NE [] :E [] :SE [] :S [] :SW [] :W [] :NW []}
             known {:N [] :NE [] :E [] :SE [] :S [] :SW [] :W [] :NW []}
             [Neven Nodd] [0 0]
             iter 0]
        (newline)
        (println iter)
        (println "maps  :" maps)
        (println "known :" known)

        (if (empty? maps)
          (if (even? Nsteps) Neven Nodd)
          (let [
                north (first (get maps :N))
                _ (println "  N maps :" (count (get maps :N)))
                _ (println "  north  :" north)
                NstepsStart (ffirst (get maps :N))
                [neven nodd new-nstep map-out Nvisited] (walk-one-map-memo (second (first (get maps :N))) gardens minmax)
                ;; _ (println " "neven nodd new-nstep )
                _ (println "    map-out" map-out)
                ntot (cond 
                       (and (< Nsteps (+ NstepsStart new-nstep)) (even? NstepsStart))
                       (apply + (map-indexed (fn [i n] (if (and (even? i) (<= i (- Nsteps NstepsStart))) n 0)) Nvisited ))
                       (and (< Nsteps (+ NstepsStart new-nstep)) (odd? NstepsStart))
                       (apply + (map-indexed (fn [i n] (if (and (odd? i) (<= i (- Nsteps NstepsStart))) n 0)) Nvisited))
                       (even? NstepsStart) neven
                       (odd?  NstepsStart) nodd
                       )
                ]
            (if (some #(= (second north) (:map %))  (get known :N))
              (println "!match at " iter "!")
              (let [new-known {:step (first north) :map (second north) :ntot ntot :nsteps new-nstep :visited Nvisited}
                    new-north (update (get map-out [0 -1]) 0 + NstepsStart)
                    _ (println "    new-known :" new-known)
                    _ (println "    new-north :" new-north)
                    ]
                (recur {:N [new-north]} (update known :N conj new-known) [Neven Nodd] (inc iter))
                ))
            ))
        
        )
        
    )
  )

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
    #_(let [[Neven Nodd nstep pos-out Nvisited] (walk-one-map {0 [pos0]} gardens minmax)
        ;;   minS (into {} (map (fn [[k v]] [k (apply min (map first v))]) pos-out))
          _ (newline)
          _ (println pos-out)
          _ (println (second (get pos-out [-1 0])))
          _ (newline)
          [Neven Nodd nstep pos-out Nvisited] (walk-one-map (second (get pos-out [-1 0])) gardens minmax)
          ]
      
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

    #_(walk-all-maps { [0 0] [0 {0 [pos0]}]} Nsteps gardens minmax)
    ;; (walk-one-map {1 '([10 7]), 0 '([10 0] [10 10] [10 4])} gardens minmax)

    (walk-with-pattern pos0 Nsteps gardens minmax)
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
  (println "10 steps => Expect 50")
  (prn (d21 sample 10))
  (newline)
  (println "50 steps => Expect 1594")
  (prn (d21 sample 50))
  (newline)
  (println "100 steps => Expect 6536")
  (prn (d21 sample 100))
;;   (newline)
;;   (println "500 steps => Expect 167004")
;;   (prn (d21 sample 500))
;;   (newline)
;;   (println "1000 steps => Expect 668697")
;;   (prn (d21 sample 1000))

;;   (newline)
;;   (println "on input:")

;;   (println "64 steps => Expect 3 733")
;;  (prn (d21 (slurp "input/day21.txt") 64))
;;   (println "100 steps => Expect 8 971")
;;  (prn (d21 (slurp "input/day21.txt") 100))
;;   (println "500 steps => Expect 220 952")
;;  (prn (d21 (slurp "input/day21.txt") 500))
;;   (println "1000 steps => Expect 881 882")
;;  (prn (d21 (slurp "input/day21.txt") 1000))
;;   (println "5000 steps => Expect 21 995 272")
;;  (time (prn (d21 (slurp "input/day21.txt") 5000)))
;;  (prn (d21 (slurp "input/day21.txt") 26501365))

  )

;; 6107040063 is too low
;; 