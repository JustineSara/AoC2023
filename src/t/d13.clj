(ns t.d13
  (:gen-class)
  (:require
   [clojure.string :as cljstr]
   [clojure.set :as cljset]))


(def sample
  "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#")


(defn parse-onepattern
  [p]
  (let [by-line (cljstr/split-lines p)
        by-col (apply map str (repeat (count (first by-line)) "") by-line)]
    [by-line by-col]
    ))

(defn is-sym?
  [pp i] ;; pp is parsed-pattern i is the indices where we look for symmetry 1 <= i <= (dec (count pp))
  (if (< 0 i (count pp))
    (let [pp- (keep-indexed #(when (< %1 i) %2) pp)
          pp+ (keep-indexed #(when (>= %1 i) %2) pp)]
      (every? identity (map = (reverse pp-) pp+)))
    nil
    )
  )

(defn get-sym-value
  [pp] ;; pp is parsed-pattern i is the indices where we look for symmetry 1 <= i <= (dec (count pp))
  (first (filter #(is-sym? pp %) (range 1 (count pp)))))

(defn part1-perpattern
  [p]
  (let [[ppl ppc] (parse-onepattern p)
        symv-c (get-sym-value ppc)]
    (if symv-c
      symv-c
      (* 100 (get-sym-value ppl)))
    )
  )



(defn is-sym-by1?
  [pp i] ;; pp is parsed-pattern i is the indices where we look for symmetry 1 <= i <= (dec (count pp))
  (if (< 0 i (count pp))
    (let [pp- (keep-indexed #(when (< %1 i) %2) pp)
          pp+ (keep-indexed #(when (>= %1 i) %2) pp)]
      (= 1 (apply + (map (fn [pp1 pp2] (count (filter identity (map not= pp1 pp2)))) (reverse pp-) pp+))))
    nil))

(defn get-sym-by1-value
  [pp] ;; pp is parsed-pattern i is the indices where we look for symmetry 1 <= i <= (dec (count pp))
  (first (filter #(is-sym-by1? pp %) (range 1 (count pp)))))

(defn part2-perpattern
  [p]
  (let [[ppl ppc] (parse-onepattern p)
        symv-c (get-sym-by1-value ppc)]
    (if symv-c
      symv-c
      (* 100 (get-sym-by1-value ppl)))))

(defn d13
  [input]
  (let [all-patterns (cljstr/split input #"\n\n")]
    #_(for [p all-patterns]
      (let [[ppl ppc] (parse-onepattern p)]
        (newline)
        (prn p)
        (println "  " (map #(is-sym-by1? ppc %) (range 1 (count ppc))))
        (println "  " (filter #(is-sym-by1? ppc %) (range 1 (count ppc))))
        (println "  " ppl)
        (println "  " (map #(is-sym-by1? ppl %) (range 1 (count ppl))))
        (println "  " (filter #(is-sym-by1? ppl %) (range 1 (count ppl))))
    ))
    
    (println "part 1:" (apply + (map part1-perpattern all-patterns)))
    (println "part 2:"(apply + (map part2-perpattern all-patterns)))
    
  ))

(defn -main
  [& args]
  (println "hello")
  (println (d13 sample))
  (println (d13 (slurp "input/day13.txt"))))