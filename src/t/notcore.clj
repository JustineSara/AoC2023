(ns t.core
  (:gen-class)
  (:require
   [clojure.string :as str]
   [clojure.set :as cljset]))


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

(defn mainD0
  []
  (println "no day 0!"))



(def day3sample
  "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(defn get-all-num
  ([idx-line line]
   (let [num (re-find #"\d+" line)]
     (if num
       (get-all-num
        idx-line
        (seq (vector [idx-line (str/index-of line num) (+ (str/index-of line num) (count num) -1) (parse-long num)]))
        (str/replace-first line (re-pattern num) (str/join "" (for [_ (range (count num))] "."))))
       [(seq ()) line])))
  ([idx-line seq-of-num line]
   (let [num (re-find #"\d+" line)]
     (if num
       (get-all-num
        idx-line
        (conj seq-of-num [idx-line (str/index-of line num) (+ (str/index-of line num) (count num) -1) (parse-long num)])
        (str/replace-first line (re-pattern num) (str/join "" (for [_ (range (count num))] "."))))
       [seq-of-num line]))))

(defn get-symbols
  ([idx-line line]
   (let [sym (re-find #"[^\.]" line)]
     (if sym
       (get-symbols
        idx-line
        (seq (vector [idx-line (str/index-of line sym)]))
        (str/replace-first line sym "."))
       (seq ()))))
  ([idx-line seq-sym line]
   (let [sym (re-find #"[^\.]" line)]
     (if sym
       (get-symbols
        idx-line
        (conj seq-sym [idx-line (str/index-of line sym)])
        (str/replace-first line sym "."))
       seq-sym))))


(defn get-stars
  ([idx-line line]
   (let [sym (re-find #"\*" line)]
     (if sym
       (get-stars
        idx-line
        (seq (vector [idx-line (str/index-of line sym)]))
        (str/replace-first line sym "."))
       (seq ()))))
  ([idx-line seq-sym line]
   (let [sym (re-find #"\*" line)]
     (if sym
       (get-stars
        idx-line
        (conj seq-sym [idx-line (str/index-of line sym)])
        (str/replace-first line sym "."))
       seq-sym))))

(defn day3part1
  [input]
  (let [lines (str/split-lines input)
        all-num-and-sym (map-indexed
                         (fn [idx-line line] ((fn [[nums line]] [nums (get-symbols idx-line line)]) (get-all-num idx-line line)))
                         lines)
        nums (apply concat (apply conj [] (map first all-num-and-sym)))
        syms (apply concat (apply conj [] (map last all-num-and-sym)))]
    ;; (println nums)
    ;; (println syms)
    ;; (println (map
    ;;           (fn [[idx-line xmin xmax value]]
    ;;             (vector value (filter (fn [[i-line-s x-s]]
    ;;                                     (and
    ;;                                      (or (= i-line-s idx-line) (= (+ i-line-s 1) idx-line) (= (- i-line-s 1) idx-line))
    ;;                                      (and (<= x-s (+ xmax 1)) (>= x-s (- xmin 1)))))
    ;;                                   syms)))
    ;;           nums))
    (apply + (map
              (fn [[idx-line xmin xmax value]]
                (if (> (count (filter (fn [[i-line-s x-s]]
                                        (and
                                         (or (= i-line-s idx-line) (= (+ i-line-s 1) idx-line) (= (- i-line-s 1) idx-line))
                                         (and (<= x-s (+ xmax 1)) (>= x-s (- xmin 1)))))
                                      syms)) 0)
                  value 0))
              nums))))



(defn day3part2
  [input]
  (let [lines (str/split-lines input)
        all-num-and-sym (map-indexed
                         (fn [idx-line line] ((fn [[nums line]] [nums (get-stars idx-line line)]) (get-all-num idx-line line)))
                         lines)
        nums (apply concat (apply conj [] (map first all-num-and-sym)))
        syms (apply concat (apply conj [] (map last all-num-and-sym)))]
    ;; (println nums)
    ;; (println syms)
    (apply + (map last
                  (filter
                   (fn [[n _]] (= n 2))
                   (map (fn [[idx-line x-s]]
                          (let [neighbors (filter
                                           (fn [[i-line xmin xmax value]]
                                             (and (or (= idx-line i-line) (= idx-line (+ i-line 1)) (= idx-line (- i-line 1)))
                                                  (and (<= x-s (+ xmax 1)) (>= x-s (- xmin 1)))))
                                           nums)]
                            (vector (count neighbors) (apply * (map last neighbors)))))
                        syms))))))

(defn mainD3
  []
  (println "Day 3 - engine schematics")
  (println day3sample)
  (println "part 1")
  (println (day3part1 day3sample))
  (println (day3part1 (slurp "input/day3.txt")))
  (println "part 2 - gear power")
  (println (day3part2 day3sample))
  (println (day3part2 (slurp "input/day3.txt"))))

(def d4sample
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(defn cardvalue
  [value n-match]
  (if (and n-match (> n-match 0))
    (if (= value 0)
      (cardvalue 1 (- n-match 1))
      (cardvalue (* value 2) (- n-match 1)))
    value))

(defn count-matchs
  [[win have]]
  (apply +
         (map (fn [h] (if (nil? (some #(= h %) win)) 0 1)) have)))

(defn d4part1
  [input]
  (let [lines (str/split-lines input)]
    (apply +
           (map (fn [l] (let [[win have] (str/split (last (str/split l #"\:")) #"\|")]
                          (cardvalue 0 (count-matchs (vector (re-seq #"\d+" win) (re-seq #"\d+" have)))))) lines))))

(defn get-card-values
  [line]
  (let [[card values] (str/split line #"\:")
        card-id (parse-long (re-find #"\d+" card))
        [win have] (map (fn [s] (re-seq #"\d+" s)) (str/split values #"\|"))]
    {:card-id card-id  :win win :have have}))


(defn count-cards
  ([dict-cards]
   (count-cards
    0 ;; N-cards
    (range 1 (+ (count dict-cards) 1)) ;; list-cards
    dict-cards))
  ([N-cards list-cards dict-cards]
   (if (> (count list-cards) 0)
     (let [card-id (apply min list-cards)
           new-list-cards (filter #(> % card-id) list-cards)
           n-cards (- (count list-cards) (count new-list-cards))
           card (first (get dict-cards card-id))
           N-matchs (count-matchs (vector (:win card) (:have card)))]

       (println card-id n-cards
                ;;    card N-matchs
                ;; (concat 
                ;;   (apply concat (repeat n-cards (filter #(<= % (count dict-cards)) (range (+ 1 card-id) (+ 1 card-id N-matchs)))))
                ;;   new-list-cards))
                )
       (count-cards
        (+ N-cards n-cards)
        (concat
         (apply concat (repeat n-cards (filter #(<= % (count dict-cards)) (range (+ 1 card-id) (+ 1 card-id N-matchs)))))
         new-list-cards)
        dict-cards))
     N-cards)))

;; could have try to use SWAP! to keep track of the number of each cards
;; nope ! seems swap is more complexe than that or you need to work with atoms
;; Care (update map key function(with previous value)) --> does not update the map but creates a new one

(defn d4part2
  [input]
  (let [lines (str/split-lines input)
        dict-cards (group-by :card-id (map get-card-values lines))]
    (count-cards dict-cards)))

(defn mainD4
  []
  (println "Day 4 - scratch cards")
  (println d4sample)
  (println (d4part1 d4sample))
  (println (d4part1 (slurp "input/day4.txt")))
  (println "part 2 - count cards")
  (println (d4part2 d4sample))
  (println (d4part2 (slurp "input/day4.txt"))))


(def d8sample1 "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)")

(def d8sample2 "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)")

(defn dir->fct
  [dir]
  (map (fn [d] (cond
                 (= d \R) last
                 (= d \L) first
                 :else (println "\ta letter is not L or R!"))) dir))

;; \R is not the same as "R" but should be use when comparing letters (?)

(defn dict-of-nodes
  [nodes]
  (apply
   sorted-map
   (mapcat
    (fn [l] (let [[k v] (str/split l #" = ")]
              [k (re-seq #"\w{3}" v)]))
    (str/split-lines nodes))))
;; this gives a map from nodes to nodes. the keys of the maps are 'symbol' (not 'keyword')
;;      might need to use (keyword XXX) to access the values

(defn d8part1
  [input]
  (let [[dir nodes] (str/split input #"\n\n")
        dirfct (dir->fct dir)
        nodesmap (dict-of-nodes nodes)]
    (newline)
    (println (first dir))
    (loop
     [step 0 node "AAA"]
      (when (= (mod step 1000) 0)
        (newline)
        (println step "," node "-->" (get nodesmap node))
        (println "               " ((nth (apply concat (repeat dirfct)) step) ["L" "R"])))
      (if (= node "ZZZ")
        step
        (recur (inc step) ((nth (apply concat (repeat dirfct)) step) (get nodesmap node)))))))


(def d8sample3 "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
")

(defn d8part2bruteforce
  [input]
  (let [[dir nodes] (str/split input #"\n\n")
        dirfct (dir->fct dir)
        nodesmap (dict-of-nodes nodes)
        startnodes (filter #(= \A (last %)) (keys nodesmap))
        ;; endnodes (filter #(= \Z (last %)) (keys nodesmap))
        ]
    (loop
      [step 0 nodes startnodes]
      (when (= (mod step 1000) 0)
        (newline)
        (println step "," nodes))
      (if (every? #(= \Z (last %)) nodes)
          step
          (recur (inc step) (map (fn [node] ((nth dirfct (mod step (count dirfct))) (get nodesmap node))) nodes))))
  ))


(defn path-to-next-Z
  [node tot-steps nodesmap dirfct]
  (loop [step 0 n node]
    (if (and (> step 0) (= \Z (last n)))
      [n step]
      (recur (inc step) ((nth dirfct (mod (+ step tot-steps) (count dirfct))) (get nodesmap n))))))


;; Taken from here : https://rosettacode.org/wiki/Least_common_multiple
(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b, (mod a b))))

(defn lcm
  [a b]
  (/ (* a b) (gcd a b)))
;; to calculate the lcm for a variable number of arguments
(defn lcmv [& v] (reduce lcm v))


(defn d8part2
  [input]
  (let [[dir nodes] (str/split input #"\n\n")
        dirfct (dir->fct dir)
        nodesmap (dict-of-nodes nodes)
        startnodes (filter #(= \A (last %)) (keys nodesmap))
        ;; endnodes (filter #(= \Z (last %)) (keys nodesmap))
        Ndir (count dirfct)
        ;; one-node (nth startnodes 3)
        ]
    (apply lcmv (for [one-node startnodes]
    ;; (println one-node "-" 0 "-" 0)
      (loop
        [iter 0
         node one-node 
         tot-steps 0
         all-paths {}]
        (let [[new-node add-steps] (path-to-next-Z node tot-steps nodesmap dirfct)
              new-tot-steps (+ tot-steps add-steps)]
          (println new-node "-" (mod new-tot-steps Ndir) "-" new-tot-steps)
          (cond
            (> iter 10) (println "too many iter")
            (and (= node new-node) (= (mod new-tot-steps Ndir) (mod tot-steps Ndir))) add-steps #_(println "min:" tot-steps "- step:" add-steps)
            :else (recur (inc iter) new-node new-tot-steps all-paths))))))))


(defn mainD8
    []
    (println "Day 8 - sandstorm")
  ;; (println d8sample1)
  ;; (println (d8part1 d8sample1))
  ;; (newline)
  ;; (println d8sample2)
  ;; (println (d8part1 d8sample2))
  ;; (newline)
  ;; (println (d8part1 (slurp "input/day8.txt")))
    (newline)
    (println "part 2 - multi time-space ghost walking")
    ;; (println d8sample3)
    ;; (println (d8part2 d8sample3))
    (newline)
    (println (d8part2 (slurp "input/day8.txt"))))


(def d5sample1
  "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

(defn parse-one-map
  [[txt & nums]]
  (let [[start end] (str/split (str/replace txt " map:" "") #"-to-")]
    [start [end (map #(map parse-long (re-seq #"-?\d+" %)) nums)]]))

(defn parse-input
  [input]
  (let [groups (str/split input #"\n\n")
        seeds (map parse-long (re-seq #"\d+" (first groups)))]
    [seeds (reduce #(assoc %1 (first %2) (last %2)) {} (map #(parse-one-map (str/split-lines %)) (rest groups)))]
  ))


(defn sold5p1-allsteps
  [value value-name maps all-steps]
  #_(println value value-name (get maps value-name))
  (if (= value-name "location")
    all-steps
    (let [[next-value-name nums] (get maps value-name)
          this-map (first (filter #(and (>= value (second %) ) (< (- value (second %)) (last %))) nums))]
      (if this-map
        (let [new-value (+ (first this-map) (- value (second this-map)))]
          (sold5p1-allsteps new-value next-value-name maps (conj all-steps new-value)))
        (sold5p1-allsteps value next-value-name maps (conj all-steps value))))
  ))

(defn sold5p1
  [value value-name maps ]
  #_(println value value-name (get maps value-name))
  (if (= value-name "location")
    value
    (let [[next-value-name nums] (get maps value-name)
          this-map (first (filter #(and (>= value (second %)) (< (- value (second %)) (last %))) nums))]
      (if this-map
        (let [new-value (+ (first this-map) (- value (second this-map)))]
          (sold5p1 new-value next-value-name maps))
        (sold5p1 value next-value-name maps)))))


(defn d5part1
  [input]
  (let [[seeds maps] (parse-input input)]
    (apply min (map #(sold5p1 % "seed" maps) seeds))))


(defn range-transform-with-one-map
  [this-range this-map]
  #_(println "     " this-range  this-map)
  (let [[start size] this-range
        [map-start-new map-start-old map-size] this-map
        end (+ start size -1)
        map-end-old (+ map-start-old map-size -1)]
      (cond
        (< end map-start-old) [[this-range] []] ;; outside of this range
        (> start map-end-old) [[this-range] []] ;; also outside of this range
        (< start map-start-old) ;; it starts before the map but end in of after
          (map conj 
               [[] []]
               [[start (+ map-start-old (- start))] []]
               (let [l (range-transform-with-one-map [map-start-old (- size (+ map-start-old (- start)))] this-map)] [(first (first l)) (last (last l))]))
        (<= end map-end-old) ;; all the range in the map
          [[] [[(+ map-start-new (- start map-start-old)) size]]]
        :else  ;; start in range, ends outside
          (let [size-in-map (inc (- map-end-old start))]
            [[[(inc map-end-old) (- size size-in-map)]] [[(+ map-start-new (- start map-start-old)) size-in-map]]])

        )))

(defn range-transf-for-reduce
  [old-and-new-ranges this-map]
  (let [old-ranges (filter #(> (count %) 0) (first old-and-new-ranges))
        new-ranges (filter #(> (count %) 0) (second old-and-new-ranges))]
    (println "   " old-ranges new-ranges)
    (loop [to-do-old-ranges old-ranges
           done-old-ranges []
           new-ranges new-ranges]
      (if (zero? (count to-do-old-ranges))
        [done-old-ranges new-ranges]
        (let [[output-old output-new] (range-transform-with-one-map (first to-do-old-ranges) this-map)]
          (recur (rest to-do-old-ranges) (concat done-old-ranges output-old) (concat new-ranges output-new))))
    )))



(defn d5part2
  [input]
  (let [[seeds maps] (parse-input input)
        seed-ranges (partition 2 seeds)]
    (apply min (map #(first %)
      (loop [this-ranges seed-ranges
             this-stage "seed"]
        (println " " this-stage this-ranges)
        (println "  maps" (get maps this-stage))
        (if (= this-stage "location")
          this-ranges
          (let [one-map (get maps this-stage)
                next-stage (first one-map)
                this-stage-maps (last one-map)]
            (recur (apply concat (reduce range-transf-for-reduce [this-ranges []] this-stage-maps)) next-stage)))
    )))))

;; I neede the prints to avoid "StackOverFlowError"
;; because otherwise a lot of stuff is lazy : it is stored as a stack of fucntions rather than in memory
;; calling the prints force it out of lazy form into memory and the stack (which is relatively speaking small) is not overflowed
;; forcing out of lazy can also be done with (vec )


(defn mainD5 
  []
  (println "Day 5 - from seed to soil to location")
  (println "part 1 - closest location")
  (println d5sample1)
  (newline)
  (println (d5part1 d5sample1))
  (newline)
  #_(println (d5part1 (slurp "input/day5.txt")))
  (println "part 2 - all the seeds because range")
  (println (d5part2 d5sample1))
  ;; (println (range-transform-with-one-map '(97 2) '(50 98 2)))
  ;; (println (reduce range-transf-for-reduce [['(15 2) '(95 20)] []] ['(50 98 2) '(0 102 2)]))
  (println (d5part2 (slurp "input/day5.txt")))
  )




(def d9sample1 "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45")

(defn get-diffs 
  [nums max-values]
  (if (every? zero? nums)
    max-values
    (get-diffs (map - (rest nums) (drop-last 1 nums)) (conj max-values (last nums)) )))

(defn d9part1
  [input]
  (let [lines (str/split-lines input)]
    (apply + (map (fn [l] (apply + (get-diffs (map parse-long (re-seq #"-?\d+" l)) []))) lines))))


(defn get-diffs-part2
  [nums max-values step-num]
  (if (every? zero? nums)
    max-values
    (get-diffs-part2 (map - (rest nums) (drop-last 1 nums))
                     (conj max-values (if (even? step-num) (first nums) (* (first nums) -1)))
                     (inc step-num))))

(defn d9part2
  [input]
  (let [lines (str/split-lines input)]
    (apply + (map (fn [l] (apply + (get-diffs-part2 (map parse-long (re-seq #"-?\d+" l)) [] 0))) lines))))

(defn mainD9
  []
  (println "Day 9")
  (println d9sample1)
  (println (d9part1 d9sample1))
  (newline)
  #_(println (d9part1 (slurp "input/day9.txt")))
  (println "part2")
  (println (d9part2 d9sample1))
  (println (d9part2 (slurp "input/day9.txt"))))

;;  1861136893 wrong => was missing "-" in number detection

(def d10sample1
  "-L|F7
7S-7|
L|7||
-L-J|
L|-JF")

(def day10sample2
"7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ")

(defn d10-parse-lines
  [lines]
  (map-indexed (fn [y l] (map-indexed (fn [x c] [x y c]) l)) lines))

(defn connected-tiles
  [[x y s]]
  (cond
    (= s \|) [[x (inc y)] [x (dec y)]]
    (= s \-) [[(inc x) y] [(dec x) y]]
    (= s \L) [[x (dec y)] [(inc x)  y]]
    (= s \J) [[x (dec y)] [(dec x) y]]
    (= s \7) [[(dec x) y] [x (inc y)]]
    (= s \F) [[(inc x) y] [x (inc y)]]
  ))


(defn get-tile
  [[x y] all-points]
  (first (filter #(= [x y] [(first %) (second %)]) all-points)))

(defn around-S
  [all-points]
  (let [[xs ys _] (first (filter #(= \S (last %)) all-points))
        options (map (fn [pos] (get-tile pos all-points)) [[xs (inc ys)] [xs (dec ys)] [(inc xs) ys] [(dec xs) ys]])
        connected (map connected-tiles options)
        ]
    (map 
      first 
      (filter 
        (fn [[_ c]] (some true? c))
        (map (fn [o c] [o (map #(= % [xs ys]) c)]) options connected)))))

(defn get-next-tile
  [[xp yp _] tile all-points]
  (let [[c1 c2] (connected-tiles tile)]
    (cond
      (= c1 [xp yp]) (get-tile c2 all-points)
      (= c2 [xp yp]) (get-tile c1 all-points)))
  )

(defn d10p1steps
  [step prevtiles tiles all-points]
  (let [[p1 p2] prevtiles
        [t1 t2] tiles]
    (when (= 0 (mod step 1000)) (prn step tiles))
    (if
      (or (= t1 t2) (= p1 t1) (= p1 t2) (= p2 t1) (= p2 t2))
      step
      (recur (inc step) tiles [(get-next-tile p1 t1 all-points) (get-next-tile p2 t2 all-points)] all-points))))

(defn d10part1
  [input]
  (let [lines (str/split-lines input)
        all-points (apply concat (d10-parse-lines lines))
        S (first (filter #(= \S (last %)) all-points))
        S-connected-tiles (around-S all-points)]
    #_(prn all-points)
    (prn S)
    (prn S-connected-tiles)
    (d10p1steps 1 [S S] S-connected-tiles all-points)))

(def d10sample3 "...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........
")

;; only left going pipe can be at the right of S
(defn S-is-actually
  [[xs ys _] [x1 y1 _] [x2 y2 _]]
  (let [cand-right (when (or (= [x1 y1] [(inc xs) ys]) (= [x2 y2] [(inc xs) ys])) (set [\- \L \F]))
        cand-top (when (or (= [x1 y1] [xs (dec ys)]) (= [x2 y2] [xs (dec ys)])) (set [\| \L \J]))
        cand-left (when (or (= [x1 y1] [(dec xs) ys]) (= [x2 y2] [(dec xs) ys])) (set [\- \J \7]))
        cand-bot (when (or (= [x1 y1] [xs (inc ys)]) (= [x2 y2] [xs (inc ys)])) (set [\| \7 \F]))
        ]
    (first (apply clojure.set/intersection (keep  identity [cand-right cand-top cand-left cand-bot])))
))

(defn d10findloop
  [prevtile tile all-tiles S all-points]
  (let [next-tile (get-next-tile prevtile tile all-points)]
    (when (= 0 (mod (count all-tiles) 1000)) (prn (count all-tiles) tile))
    (if
      (= next-tile S)
      all-tiles
      (recur tile next-tile (conj all-tiles next-tile) S all-points))))


(defn all-inside-candidate-col
  [[list-candidates open previous-y] [x y s]]
  (prn y list-candidates open previous-y s)
  (cond
    (= s \|) [list-candidates open y]
    (or (= s \L) (= s \J)) [list-candidates true y]
    (or (= s \F) (= s \7)) (if open [(concat list-candidates (map #(vector % x) (range (inc previous-y) y))) false y] [list-candidates false y])
    (= s \-) (if open [(concat list-candidates (map #(vector x %) (range (inc previous-y) y))) false y] [list-candidates true y])
    ))

#_(defn all-inside-candidate-lig
  [[list-candidates open previous-x previous-s] [x y s]]
  ;; (prn y list-candidates open previous-y s)
  (if 
    open
    (cond
      (= s \-) [list-candidates open x previous-s] ;; I need  to propagate the information about how I "open" the first time
      (= s \|) [(concat list-candidates (map #(vector % y) (range (inc previous-x) x))) false x s]
      (or (= s \F) (= s \L)) [(concat list-candidates (map #(vector % y) (range (inc previous-x) x))) true x s]
      )
    (cond
      (= s \-) [list-candidates open x previous-s]
      (= s \|) [list-candidates true x s]
      (or (= s \F) (= s \L) (= s \|)) [list-candidates true x s]
      (= s \J) []
      (or (= s \7) ) [list-candidates true x]
       (if open [(concat list-candidates (map #(vector % x) (range (inc previous-y) y))) false y] [list-candidates false y])
      (= s \|) (if open [(concat list-candidates (map #(vector % y) (range (inc previous-x) x))) false x] [list-candidates true x]))))

(defn transform-path-y
  [path-y clean-path-y]
  (if
    (= (count path-y) 0)
    clean-path-y
    (let [[x1 _ s1] (first path-y)
          path-y (rest path-y)]
      (if
        (= s1 \|)
        (recur path-y (conj clean-path-y [s1 x1 x1]))
        (let [[x2 _ s2] (first path-y)
              path-y (rest path-y)]
          (if
            (or (and (= s1 \L) (= s2 \J)) (and (= s1 \F) (= s2 \7)))
            (recur path-y clean-path-y)
            (recur path-y (conj clean-path-y [(str s1 s2) x1 x2]))
            ))
      )
      ))
  )

(defn xy-inside
  [y elems]
  (if 
    (zero? (count elems))
    []
    (let [eles (partition 2 (sort-by second elems))]
      (map (fn [x] [x y]) (mapcat (fn [[[_ start _][_ _ end]]] (range (inc start) end)) eles)))
    ))

(defn d10part2
  [input]
  (let [lines (str/split-lines input)
        all-points (apply concat (d10-parse-lines lines))
        S (first (filter #(= \S (last %)) all-points))
        S-connected-tiles (around-S all-points)
        s-symb (S-is-actually S (first S-connected-tiles) (second S-connected-tiles))
        all-points (map (fn [[x y s]] [x y (if (= s \S) s-symb s)]) all-points)
        S [(first S) (second S) s-symb]
        all-path-points (d10findloop S (first S-connected-tiles) [S (first S-connected-tiles)] S all-points)
        ;; path-by-y (group-by second (d10findloop S (first S-connected-tiles) [S (first S-connected-tiles)] S all-points))
        transformed-paths (apply sorted-map (mapcat (fn [[k v]] [k (transform-path-y (sort-by first (filter #(not= (last %) \-) v)) [])]) (group-by second all-path-points)))
        candidates (set (mapcat (fn [[k v]] (xy-inside k v)) transformed-paths))]
    (prn S)
    ;; (prn all-path-points)
    ;; ;; (prn (sort-by first (map (fn [[k v]] [k (reduce count-inside [0 false -1] (sort-by second v))]) path)))
    ;; ;; (prn transformed-paths)
    ;; ;; (prn (xy-inside 6 (get transformed-paths 6) ))
    ;; (prn candidates)
    ;; (prn (reduce count-inside [0 false -1] (sort-by second (get path 16))))
    ;; (apply + (map (fn [[_ v]] (first (reduce count-inside [0 false -1] (sort-by second v)))) path))
    (count (cljset/difference candidates (set (map (fn [[x y _]] [x y]) all-path-points))))
  ))

(def d10sample4
  "FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L")

(defn mainD10
  []
  (println "Day 10")
  ;; (println d10sample1)
  ;; (println (d10part1 d10sample1))
  ;; (newline)
  ;; (println (d10part1 day10sample2))
  ;; (newline)
  ;; (println (d10part1 (slurp "input/day10.txt")))
  (println "part2")
  (println d10sample3)
  (prn (d10part2 d10sample3))
  (println d10sample4)
  (prn (d10part2 d10sample4))
  (newline)
  (println (d10part2 (slurp "input/day10.txt"))))



(def d7sample1 "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
")


(defn hand-type
  [cards]
  (let [cards-nums (sort > (map (fn [[_ v]] (count v)) (group-by identity cards)))
        max-cn (first cards-nums)
        sec-max-cn (second cards-nums)]
    (cond
      (= max-cn 5) 6
      (= max-cn 4) 5
      (= max-cn 3) (if (= sec-max-cn 2) 4 3)
      (= max-cn 2) (if (= sec-max-cn 2) 2 1)
      :else 0
    )))

(defn make-hand
  [line]
  (let [[cards bid] (str/split line #" ")
        bid (parse-long bid)]
    {:cards cards
     :bid bid
     :type (hand-type cards)}))

;; lesson : convention is to go from smaller to higher rather than what I did ... oups

(defn x-higher-cards-than-y
  ;; A K Q J T 9 8 7 6 5 4 3 2
  ([x]
    true)
  ([x y]
    (cond
      (= x y) true
      (= x \A) true
      (= y \A) false
      (= x \K) true
      (= y \K) false
      (= x \Q) true
      (= y \Q) false
      (= x \J) true
      (= y \J) false
      (= x \T) true
      (= y \T) false
      :else (let [x (parse-long (str x)) y (parse-long (str y))] (> x y))
      ))
  ([x y & more]
    (let [all (concat [x y] more)]
      (every? true? (map #(x-higher-cards-than-y (first %) (second %)) (partition 2 1 all)))
    )
   ))

(defn x-higher-cardhand-than-y
  ([x] true)
  ([x y]
    (first (filter boolean? (map (fn [c1 c2] (if (= c1 c2) nil (x-higher-cards-than-y c1 c2))) x y))))
  ([x y & more]
    (let [all (concat [x y] more)]
      (every? true? (map #(x-higher-cardhand-than-y (first %) (second %)) (partition 2 1 all))))))

(defn x-higher-hand-than-y
  ([x] true)
  ([x y]
    (cond 
      (> (get x :type) (get y :type)) true
      (< (get x :type) (get y :type)) false
      :else (x-higher-cardhand-than-y (get x :cards) (get y :cards))))
  ([x y & more]
    (let [all (concat [x y] more)]
      (every? true? (map #(x-higher-hand-than-y (first %) (second %)) (partition 2 1 all))))))

(defn d7part1
  [input]
  (let [lines (str/split-lines input)]
    #_(prn (make-hand (first lines)))
    #_(prn (make-hand (second lines)))
    #_(prn (x-higher-cards-than-y \K \Q))
    #_(prn (x-higher-cards-than-y \K \A))
    #_(prn (sort x-higher-cards-than-y [\K \Q \A \J \3 \2 \K \T] ))
    #_(prn (sort x-higher-cardhand-than-y (map (fn [line] (first (str/split line #" "))) lines)))
    #_(map (fn [h r] (assoc h :rank r)) (reverse (sort x-higher-hand-than-y (map make-hand lines))) (range 1 (inc (count lines))))
    (apply + (map (fn [h r] (* r (get h :bid))) (reverse (sort x-higher-hand-than-y (map make-hand lines))) (range 1 (inc (count lines)))))
    ))

(defn hand-type-v2
  [cards] 
  (let [cards-dict (into {} (map (fn [[k v]] [k (count v)]) (group-by identity cards)))
        J-num (get cards-dict \J 0)
        cards-nums (sort > (map (fn [[_ v]] v) (dissoc cards-dict \J)))
        max-cn (if (= J-num 5) 5 (+ J-num (first cards-nums)))
        sec-max-cn (second cards-nums)]
    (cond
      (= max-cn 5) 6
      (= max-cn 4) 5
      (= max-cn 3) (if (= sec-max-cn 2) 4 3)
      (= max-cn 2) (if (= sec-max-cn 2) 2 1)
      :else 0)))

(defn make-hand-v2
  [line]
  (let [[cards bid] (str/split line #" ")
        bid (parse-long bid)]
    {:cards cards
     :bid bid
     :type (hand-type-v2 cards)}))

(defn h-cards-v2
  ;; A K Q T 9 8 7 6 5 4 3 2 J
  ([x]
   true)
  ([x y]
   (let [x (if (= x \J) \0 x)
         y (if (= y \J) \0 y) ]
     (cond
       (= x y) true
       (= x \A) true
       (= y \A) false
       (= x \K) true
       (= y \K) false
       (= x \Q) true
       (= y \Q) false
       (= x \T) true
       (= y \T) false
       :else (let [x (parse-long (str x)) y (parse-long (str y))] (> x y)))))
  ([x y & more]
   (let [all (concat [x y] more)]
     (every? true? (map #(h-cards-v2 (first %) (second %)) (partition 2 1 all))))))

(defn h-cardhand-v2
  ([x] true)
  ([x y]
   (first (filter boolean? (map (fn [c1 c2] (if (= c1 c2) nil (h-cards-v2 c1 c2))) x y))))
  ([x y & more]
   (let [all (concat [x y] more)]
     (every? true? (map #(h-cardhand-v2 (first %) (second %)) (partition 2 1 all))))))

(defn h-hand-v2
  ([x] true)
  ([x y]
   (cond
     (> (get x :type) (get y :type)) true
     (< (get x :type) (get y :type)) false
     :else (h-cardhand-v2 (get x :cards) (get y :cards))))
  ([x y & more]
   (let [all (concat [x y] more)]
     (every? true? (map #(h-hand-v2 (first %) (second %)) (partition 2 1 all))))))

(defn d7part2
  [input]
  (let [lines (str/split-lines input)]
    ;; (map (fn [h r] (assoc h :rank r)) (reverse (sort h-hand-v2 (map make-hand-v2 lines))) (range 1 (inc (count lines))))
    (apply + (map (fn [h r] (* (get h :bid) r)) (reverse (sort h-hand-v2 (map make-hand-v2 lines))) (range 1 (inc (count lines)))))
    ))

(defn mainD7
  []
  (println "Day 7")
  (println d7sample1)
  ;; (prn (d7part1 d7sample1))
  ;; (prn (d7part1 (slurp "input/day7.txt")))
  (println "part 2")
  (prn (d7part2 d7sample1))
  (prn (d7part2 (slurp "input/day7.txt")))
  )

;;(slurp "input/day10.txt")


(def d11sample1 "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....")


(defn get-galaxies
  [il l]
  (keep identity (map-indexed (fn [ic c] (when (= c \#) [ic il])) l)))

(defn  expand-univers
  [galaxies]
  (let [xs (cljset/difference (set (range (apply max (map first galaxies)))) (set (map first galaxies)))
        ys (cljset/difference (set (range (apply max (map second galaxies)))) (set (map second galaxies)))]
    #_(prn xs)
    #_(prn ys)
    (loop [g galaxies g-expanded []]
      (if (zero? (count g))
        g-expanded
        (let [[gx gy] (first g)
              Nx (count (filter #(< % gx) xs))
              Ny (count (filter #(< % gy) ys))] 
           (recur (rest g) (conj g-expanded [(+ gx Nx) (+ gy Ny)]))))
    )))

(defn manhattan-dist
  [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))


(defn get-dist
  [[dist galaxies] g]
  (if (zero? (count galaxies))
    [dist []]
    [(+ dist (apply + (map #(manhattan-dist g %) galaxies)))
     (rest galaxies)]))

(defn d11part1
  [input]
  (let [lines (str/split-lines input)
        galaxies (expand-univers (apply concat (map-indexed get-galaxies lines)))]
    #_(prn (sort-by second galaxies))
    (first (reduce get-dist [0 (rest galaxies)] galaxies))
    ))


(defn expand-univers-multi
  [galaxies multi]
  (let [xs (cljset/difference (set (range (apply max (map first galaxies)))) (set (map first galaxies)))
       ys (cljset/difference (set (range (apply max (map second galaxies)))) (set (map second galaxies)))]
   #_(prn xs)
   #_(prn ys)
   (loop [g galaxies g-expanded []]
     (if (zero? (count g))
       g-expanded
       (let [[gx gy] (first g)
             Nx (count (filter #(< % gx) xs))
             Ny (count (filter #(< % gy) ys))]
         (recur (rest g) (conj g-expanded [(+ gx (* Nx multi) (- Nx)) (+ gy (* Ny multi) (- Ny))])))))))

(defn d11part2
  [input multi]
  (let [lines (str/split-lines input)
        galaxies (apply concat (map-indexed get-galaxies lines))
        exp-g (expand-univers-multi galaxies multi)]
    ;; (prn (sort-by second galaxies))
    ;; (prn (sort-by second (expand-univers galaxies)))
    ;; (prn (sort-by second exp-g))
    (first (reduce get-dist [0 (rest exp-g)] exp-g))
  ))

;; 678626878012 too high


(defn mainD11
  []
  (println "Day 11")
  (println d11sample1)
  (println (d11part1 d11sample1))
  #_(println (d11part1  (slurp "input/day11.txt")))
  (println "part 2")
  (println (d11part2 d11sample1 10))
  (println (d11part2 d11sample1 100))
  (println (d11part2 (slurp "input/day11.txt") 1000000)))

