(ns t.d22
  (:gen-class)
  (:require
   [clojure.string :as cljstr]
   [clojure.set :as cljset]))

(def sample "1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9")

(defn parse-input
  [input]
  (->>
  (cljstr/split-lines input)
   (map (fn [l] (let [[B E](cljstr/split l #"~")
                      [x1 y1 z1] (cljstr/split B #",")
                      [x2 y2 z2] (cljstr/split E #",")]
                  [[(parse-long x1) (parse-long x2)] [(parse-long y1) (parse-long y2)] [(parse-long z1) (parse-long z2)]])))
   )
)

(defn draw_slice_x
  [bricks]
  (->>
   (for [[[x1 x2] [y1 y2] [z1 z2]] bricks
         x (range x1 (inc x2))
         z (range z1 (inc z2))]
     [z x])
   set
   ((fn [pts] [(apply min (map second pts)) (apply max (map second pts))
               (apply min (map first pts)) (apply max (map first pts))
               pts]))
   ((fn [[minX maxX minZ maxZ pts]] (map (fn [z] (apply str (map (fn [x] (if (contains? pts [z x]) \# \.)) (range minX (inc maxX))))) (range minZ (inc maxZ)))))
   (reverse)
   (cljstr/join "\n")
   )  
  )

#_(defn one-brick-fall
  [[[x1 x2] [y1 y2] [z1 z2]] floor]
  (if (empty? floor)
    [[[x1 x2] [y1 y2] [1 (- z2 (- z1 1))]]
     (into {} (mapcat (fn [y] (map (fn [x] [[x y] (- z2 (- z1 1))]) (range x1 (inc x2)))) (range y1 (inc y2))))]
    (let [Z (apply min
                   (for [x (range x1 (inc x2))
                         y (range y1 (inc y2))]
                     (get floor [x y] 0)))])))

(defn one-brick-fall
  [brick fallen]
  ;; (println " " brick )
  (let [[[x1 x2] [y1 y2] [z1 z2]] brick
        support (filter (fn [[[xf1 xf2] [yf1 yf2] _]] (and (<= x1 xf2) (<= xf1 x2) (<= y1 yf2) (<= yf1 y2))) (keys fallen))
        ]
    (if (empty? support)
      (let [[[x1 x2] [y1 y2] [z1 z2]] brick
            brick [[x1 x2] [y1 y2] [1 (- z2 (- z1 1))]]]
        (assoc fallen brick {"supported" ["floor" "floor"] "supports" []}))
      (let [Z (apply max (map (fn [[_ _ [_ zf2]]] zf2) support))
            support (filter (fn [[_ _ [_ zf2]]] (= Z zf2)) support)
            fallen (reduce #(apply assoc-in %1 %2) fallen (map (fn [fb] [[fb "supports"]  (conj (get-in fallen [fb "supports"]) brick)]) support))
            brick [[x1 x2] [y1 y2] [(inc Z) (- z2 (- z1 (inc Z)))]]
            ;; _ (println "  --> " brick)
            ]
        (assoc fallen brick {"supported" support "supports" []})))))

(defn all-bricks-fall
  [bricks-in-air bricks-fallen]
  ;; bricks-in-air is already ordered
  (if (empty? bricks-in-air)
    bricks-fallen
    (recur (rest bricks-in-air) (one-brick-fall (first bricks-in-air) bricks-fallen) )
  ))


(defn chain-reaction
  [bricks-still-here Nfallen]
  ;; (println "  " Nfallen)
  ;; (println "  " (keys bricks-still-here))
  (let [bricks-falling (filter (fn [[b {spted "supported"}]] (empty? (filter (fn [bs] (or (contains? bricks-still-here bs) (= bs "floor"))) spted)) )  bricks-still-here)
        ;; _ (println "    " bricks-falling)
        ]
    (if (empty? bricks-falling)
      Nfallen
      (recur (apply dissoc bricks-still-here (keys bricks-falling))
             (+ Nfallen (count bricks-falling)))
  )))

(defn d22p1
  [input]
  (let [bricks (parse-input input)
        N-bricks (count bricks)
        bricks (sort-by (fn [[_ _ [z1 _]]] z1) bricks)
        ;; b1 (first bricks)
        f-bricks (all-bricks-fall bricks {})]
    ;; (println (draw_slice_x bricks))
    ;; (newline)
    ;; (println (draw_slice_x (keys f-bricks)))
    (->> f-bricks
         (map (fn [[_ {sped "supported"}]] (when (= (count sped) 1) (first sped) ) ) )
         (keep identity)
         set
         count
         (- N-bricks)
         )
    
    ))

(defn d22p2
  [input]
  (let [bricks (parse-input input)
             N-bricks (count bricks)
             bricks (sort-by (fn [[_ _ [z1 _]]] z1) bricks)
             f-bricks (all-bricks-fall bricks {})]
        (->> f-bricks
         (map (fn [[_ {sped "supported"}]] (when (= (count sped) 1) (first sped))))
         (keep identity)
         set
             (map (fn [b] (chain-reaction (dissoc f-bricks b) 0)))
             (apply +)
             )
    
    ;; (println (draw_slice_x (keys f-bricks)))
    ;; (println [[0 2] [1 1] [4 4]])
    ;; (chain-reaction (dissoc f-bricks [[0 2] [1 1] [4 4]]) 0)
  ))

(defn -main
  [& args] 
  (println "day22")
  (println sample)
  (newline)
  
  (println "part1")
  (prn (d22p1 sample))
;;  (prn (d22p1 (slurp "input/day22.txt")))
  
 (newline)
 (println "part2")
 (prn (d22p2 sample))
 (prn (d22p2 (slurp "input/day22.txt")))
 
  )
