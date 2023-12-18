(ns t.d18
  (:gen-class)
  (:require
   [clojure.string :as cljstr]
   [clojure.set :as cljset]))

(def sample "R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)")

(def sample2 "L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
R 6 (#70c710)
D 5 (#0dc571)")

(def sample3 "R 6 (#5713f0)
D 3 (#d2c081)
L 2 (#59c680)
D 3 (#411b91)
L 2 (#8ceee2)
U 3 (#caa173)
L 2 (#1b58a2)
U 3 (#caa171)")
(def sample4 
"R 4
D 5
L 3
D 4
L 3
U 6
R 2
U 3")

(def sample5  "R 5
D 3
L 3
D 2
R 6
U 3
R 2
D 6
L 12
U 4
R 2
U 4")

(def sample6  "R 7
D 6
L 2
U 3
L 4
D 3
L 1
U 6")

(defn parse-input
  [input]
  (map (fn [l] (let [ll (cljstr/split l #" ")] [(first ll) (parse-long (second ll))])) (cljstr/split-lines input)))

(defn dig-one
  [[x y] [dir nm]]
  (cond
    (= dir "R") (map (fn [n] [(+ x n) y]) (range 1 (inc nm)))
    (= dir "L") (map (fn [n] [(- x n) y]) (range 1 (inc nm)))
    (= dir "D") (map (fn [n] [x (+ y n)]) (range 1 (inc nm)))
    (= dir "U") (map (fn [n] [x (- y n)]) (range 1 (inc nm)))))
    
  

(defn get-border
  [moves]
  (loop [pos [0 0]
         all-pos [[0 0]]
         mvs moves]
    (if (empty? mvs)
      all-pos
      (let [[mv & mvs] mvs
            new-pos (dig-one pos mv)]
        (recur (last new-pos) (concat all-pos new-pos) mvs)))))
      
    
  

(defn print-border
  [border]
  (let [border (set border)
        minx (apply min (map first border))
        maxx (apply max (map first border))
        miny (apply min (map second border))
        maxy (apply max (map second border))]
    (apply str
           (for [y (range miny (inc maxy))
                 x (range minx (inc maxx))]
            (if (contains? border [x y])
              (str "#" (when (= x maxx) "\n"))
              (str "." (when (= x maxx) "\n")))))))
      
    
  


(defn one-point-inside
  [borders]
  (let [miny (apply min (map second borders))
        b-miny (filter #(= miny (second %)) borders)
        ;; _ (prn b-miny)
        b-miny+1 (filter #(= (inc miny) (second %)) borders)]
        ;; _ (prn b-miny+1)
        
    (->>
     b-miny+1
     (sort-by first)
     first
     ((fn [[x y]] [(inc x) y])))))
     
    ;; SHOULD BE CHECKING that it is indeed inside using b-miny but checked by eye for my case
  

(defn crawl-inside
  [borders]
  (let [borders (set borders)
        start-pt (one-point-inside borders)]
    (loop [to-exp [start-pt]
           pt-in #{start-pt}]
      (if (empty? to-exp)
        (+ (count pt-in) (count borders))
        (let [[[x y] & to-exp] to-exp
              new-pt (for [dx (range -1 2) dy (range -1 2)] [(+ x dx) (+ y dy)])
              new-pt (filter #(not (contains? pt-in %)) new-pt)
              new-pt (filter #(not (contains? borders %)) new-pt)]
              
          (recur (concat to-exp new-pt) (apply conj pt-in new-pt)))))))
          
        
     
     
  
  
;; unused
(defn opp-dir
  [dir]
  (cond 
    (= dir "R") "L"
    (= dir "L") "R"
    (= dir "U") "D"
    (= dir "D") "U"))
    

(defn rotate-moves
  [moves]
  (let [[newm miny]
    (loop [mv moves nmv [] y 0 miny 0]
      (if (empty? mv)
        [nmv miny]
        (let [[[d n] & mv] mv]
          (cond 
            (or (= d "L") (= d "R"))
            (recur mv (concat nmv [[d n y]]) y miny)
            (= d "U")
            (recur mv (concat nmv [[d n (- y n)]]) (- y n) (min miny (- y n)))
            (= d "D")
            (recur mv (concat nmv [[d n (+ y n)]]) (+ y n) miny)
          )
      )))
        [lmv [lorf-mv & fm]] (split-with #(> (last %) miny) newm)]
    (if
     (or (= (first lorf-mv) "R") (= (first lorf-mv) "L"))
     (map (fn [m] [(first m) (second m)]) (concat [lorf-mv] fm lmv))
     (map (fn [m] [(first m) (second m)]) (concat fm lmv [lorf-mv])))
  ))

;; assume alternances of U/D and L/R so do not considere the cases where dy1 and dx can be = or opposites
(defn surf-from-squares
  [moves S]
  (if (<= (count moves) 2)
    (inc S)
  (let [[[Dx x] & moves] moves
        [[Dy1 y1] & moves] moves
        [[Dy2 y2] moves] ((juxt last drop-last) moves)]
    ;; (newline)
    ;; (println "  surface:" S)
    ;; (println " " Dy2 y2)
    ;; (println " " Dx x)
    ;; (println " " Dy1 y1)
    (if
      (= Dy1 Dy2) (let [moves (concat [[Dy1 y1]] moves [[Dy2 y2] [Dx x] ])]
                    (recur moves S))
      (let [y (min y1 y2)
            S (+ S (* (inc x) y))
            y1 (- y1 y)
            y2 (- y2 y)]
        (if (zero? y1 ) 
          (let [[[dx xn] & moves] moves]
            ;; (println "    y1 = 0")
            ;; (println "    directions :" Dx dx)
            (if (= dx Dx) 
              (recur (concat [[Dx (+ x xn)]] moves [[Dy2 y2]]) S)
              (recur (concat [[Dx (- x xn)]] moves [[Dy2 y2]]) (+ S xn))))
          (let [[[dx xn] moves] ((juxt last drop-last) moves)]
            ;; (println "    y2 = 0")
            ;; (println "    directions :" Dx dx)
           (if (= dx Dx)
             (recur (concat [[Dx (+ x xn)] [Dy1 y1]] moves) S)
             (recur (concat [[Dx (- x xn)] [Dy1 y1]] moves) (+ S xn))))
        )
        )
      ))
    ))
      
    
  


(defn d18
  [input]
  (let [moves (parse-input input)
        borders (get-border moves)]
    (crawl-inside borders)))
    


(defn d18-p1-reviewed
  [input]
  (let [moves (parse-input input)
        ;; _ (prn moves)
        moves (rotate-moves moves)
        ;; _ (prn moves)
        ]
    (surf-from-squares moves 0)
    ))

(defn -main
  [& args]
  (println "day18")
;;   (println sample)
;;   (prn (d18 sample))
;;   (prn (d18-p1-reviewed sample))
;;   (prn (d18-p1-reviewed sample2))

;;   (println (print-border (get-border (parse-input sample2))))
  
  (println "sample 6 shows why ''surfaces from squares'' don't work")
  (println (print-border (get-border (parse-input sample6))))
  (prn (d18-p1-reviewed sample6))
  (println "result should be 47")

  (newline)
;;   (prn (d18 (slurp "input/day18.txt")))
;;   (prn (d18-p1-reviewed (slurp "input/day18.txt")))


;;   (newline)
;;   (prn (d18p2 sample))
;;   (newline)
;;   (println (d18p2 (slurp "input/day18.txt")))
  )
;; part 1 : 73014 is too high