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

(defn parse-input
  [input]
  (map (fn [l] (let [ll (cljstr/split l #" ")] [(first ll) (parse-long (second ll))])) (cljstr/split-lines input)))

(defn dig-one
  [[x y] [dir nm]]
  (cond
    (= dir "R") (map (fn [n] [(+ x n) y]) (range 1 (inc nm)))
    (= dir "L") (map (fn [n] [(- x n) y]) (range 1 (inc nm)))
    (= dir "D") (map (fn [n] [x (+ y n)]) (range 1 (inc nm)))
    (= dir "U") (map (fn [n] [x (- y n)]) (range 1 (inc nm)))
    )
  )

(defn get-border
  [moves]
  (loop [pos [0 0]
         all-pos [[0 0]]
         mvs moves]
    (if (empty? mvs)
      all-pos
      (let [[mv & mvs] mvs
            new-pos (dig-one pos mv)]
        (recur (last new-pos) (concat all-pos new-pos) mvs))
      )
    )
  )

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
        (str "." (when (= x maxx) "\n")))
      ))
    )
  )


(defn one-point-inside
  [borders]
  (let [miny (apply min (map second borders))
        b-miny (filter #(= miny (second %)) borders)
        ;; _ (prn b-miny)
        b-miny+1 (filter #(= (inc miny) (second %)) borders)
        ;; _ (prn b-miny+1)
        ]
    (->>
     b-miny+1
     (sort-by first)
     first
     ((fn [[x y]] [(inc x) y]))
     )
    ;; SHOULD BE CHECKING that it is indeed inside using b-miny but checked by eye for my case
  ))

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
              new-pt (filter #(not (contains? borders %)) new-pt)
              ]
          (recur (concat to-exp new-pt) (apply conj pt-in new-pt))
          )
        )
     
     )

  
  )

  )


(defn d18
  [input]
  (let [moves (parse-input input)
        borders (get-border moves)]
    (crawl-inside borders)
    ))

(defn -main
  [& args]
  (println "day18")
  (println sample)
  (prn (d18 sample))
  (newline)
  (prn (d18 (slurp "input/day18.txt")))
;;   (newline)
;;   (prn (d18p2 sample))
;;   (newline)
;;   (println (d18p2 (slurp "input/day18.txt")))
  )

;; part 1 : 73014 is too high