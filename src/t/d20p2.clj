(ns t.d20p2
  (:gen-class)
  (:require
   [clojure.string :as cljstr]
   [clojure.set :as cljset]))

(def sample "broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a")

(def sample2 "broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output")


(defn parse-module
  [module]
  ;; input : the string
  ;; output : [name type]
  (if (= module "broadcaster")
    ["broadcaster" "broadcaster"]
    [(subs module 1) (first module)]))

(defn init-state-for-conj
  [machine c]
  (->> machine
       (map (fn [[name info]] (when (some #(= c %) (get info "d")) [name false])))
       (keep identity)
       (into {})
       (assoc-in machine [c "s"])))

(defn parse-input
  [input]
  (->>
   (cljstr/split-lines input)
   (map (fn [l] (cljstr/split l #" -> ")))
   (map (fn [[module destinations]]  [(parse-module module) (cljstr/split destinations #", ")]))
   (map (fn [[[name type] dest]] [name {"t" type "d" dest "s" (when (= type \%) false)}]))
   (into {})))


(defn signal-through-1-module
  [from-m signal-in module]
  ;; from-m is the origine of the signal
  ;; signal-in is true (high signal) or false (low signal)
  ;; output : [signals-out module]
  ;;          with signals-out a list of ["module-name" signal]
  ;;          and  module the module with updated state
  ;; (prn module)
  (let [type (get module "t")
        dest (get module "d")
        state (get module "s")
        ;; _ (prn type dest state)
        ]
    (case type
      "broadcaster"
      [(map (fn [d] [d signal-in]) dest) module]
      \%
      (if (not signal-in)
        (let [new-state (not state)]
          [(map (fn [d] [d new-state]) dest) (assoc module "s" new-state)])
        [[] module])
      \&
      (let [new-state (assoc state from-m signal-in)]
        [(map (fn [d] [d (not (reduce #(and %1 %2) (vals new-state)))]) dest) (assoc module "s" new-state)])
      [[] module])))


(defn one-button-push
  ([machine-in]
   (loop [signals [["button" false "broadcaster"]] ;; list of signals to be treated
          Ntotal 0
          Nhigh 0
          machine machine-in]
     (if (empty? signals)
       [machine Ntotal Nhigh]
       (let [[[from-m signal to-m] & signals] signals
             [new-signals module] (signal-through-1-module from-m signal (get machine to-m))]
        ;; (println "  signal:" from-m "--" signal "-->" to-m)
        ;; (println "  generates:" new-signals)
        ;; (println (assoc machine to-m module))
         (recur
          (concat signals (map (fn [[dest sign]] [to-m sign dest]) new-signals))
          (inc Ntotal)
          (if signal (inc Nhigh) Nhigh)
          (assoc machine to-m module))))))
  ([machine Ntot Nhigh]
   (let [[new-machine addNtot addNhigh] (one-button-push machine)]
     [new-machine (+ addNtot Ntot) (+ addNhigh Nhigh)])))

(defn bruteforce
  [machine-in ntimes]
  (reduce (fn [i _] (apply one-button-push i)) [machine-in 0 0] (range ntimes)))


;; a very bad idea : there are some infinte loop if you don't considere the gates that stops the flow
(defn draw
  [machine]
  (prn machine)
  (loop [ns ["broadcaster"]]
    (when (not (empty? ns))
      (let [[n & ns] ns
            module (get machine n)
            ;; _ (prn module)
            type (get module "t")
            dest (get module "d")
            state (get module "s")
            ;; _ (prn dest)
            ;; nextn (reduce (fn [nn d] (concat nn [[d (+ (second (last nn)) 7 (- (count (first (last nn)))))]])) [[(first dest) nblank]] (rest dest))
            ]
        ;; (println (str (apply str (repeat nblank " ")) type n))
        ;; (println (str (apply str (repeat nblank " ")) "|"))
        ;; (println (str (apply str (repeat nblank " ")) "|" (apply str (repeat (* 7 (dec (count dest))) "-"))))
        ;; (println (str (apply str (repeat nblank " ")) (apply str (repeat (count dest) "|      "))))
        ;; ;; (println (apply str (apply str (repeat nblank " ")) dest))
        ;; (prn nextn)
        ;; (prn (concat ns nextn))
        ;; ;; (recur (concat ns nextn)))
        ;; (recur [])
        
        (println n "("type") --> " (apply str (cljstr/join "  " dest)))
        (recur (concat ns dest))
        ))
  ))


(defn check-patterns
  [to-be-found-patterns new-patterns]
  (->>
   to-be-found-patterns
   (map (fn [[k v]]
          (let [testing (concat [(get new-patterns k)] (reverse v))]
            (if (some identity (get new-patterns k))
              (loop [p-size 1]
                (let [[part2 part1-offset] (split-at p-size testing)]
                  (cond
                    (< (count part1-offset) (count part2))
                    {:found "noo" "module" k "pattern" (reverse testing)}
                    (every? identity (map (fn [p2 p1] (= p2 p1)) part2 part1-offset))
                    {:found "yes" "module" k "pattern" {"size" p-size
                                                        "offset" (- (count testing) (* 2 p-size))
                                                        "p" (reverse part2)}}
                    :else
                    (recur (inc p-size)))))
              {:found "noo" "module" k "pattern" (reverse testing)}))))
   (group-by :found)
   ((fn [p] [
             (into {} (map (fn [d] [(get d "module") (get d "pattern")]) (get p "yes")))
             (into {} (map (fn [d] [(get d "module") (get d "pattern")]) (get p "noo")))
             ]))
  ))

(defn one-button-push-with-patterns
  ([machine-in found-patterns to-be-found-patterns iter]
   (loop [signals [["button" false "broadcaster"]] ;; list of signals to be treated
          machine machine-in
          new-patterns (reduce #(assoc %1 %2 []) {} (keys to-be-found-patterns))]
     (if
      (empty? signals)
       (let [[n-found-patterns to-be-found-patterns] (check-patterns to-be-found-patterns new-patterns)]
         [machine (merge found-patterns n-found-patterns) to-be-found-patterns (inc iter)])
       (let [[[from-m signal to-m] & signals] signals
             [new-signals module] (signal-through-1-module from-m signal (get machine to-m))
             new-patterns (if (contains? new-patterns to-m) (assoc new-patterns to-m (concat (get new-patterns to-m) [(last (first new-signals))])) new-patterns)]
        ;;  (println "  signal:" from-m "--" signal "-->" to-m)
         (recur
          (concat signals (map (fn [[dest sign]] [to-m sign dest]) new-signals))
          (assoc machine to-m module)
          new-patterns
          ))))))


(defn one-button-push-first-time-true
  [machine-in never-true once-true iter]
   (loop [signals [["button" false "broadcaster"]] ;; list of signals to be treated
          machine machine-in
          nev-t never-true
          onc-t once-true]
     (if
      (empty? signals)
       [machine nev-t onc-t (inc iter)]
       (let [[[from-m signal to-m] & signals] signals
             [new-signals module] (signal-through-1-module from-m signal (get machine to-m))]
        ;;  (println "  signal:" from-m "--" signal "-->" to-m)
         (cond
           (and (last (first new-signals))  (contains? nev-t to-m))
           (let [nev-t (disj nev-t to-m)
                 onc-t (conj onc-t to-m)]
             (println "FIRST TRUE for " to-m "on iter " iter)
             (recur
              (concat signals (map (fn [[dest sign]] [to-m sign dest]) new-signals))
              (assoc machine to-m module)
              nev-t onc-t))
           (and (last (first new-signals))  (contains? onc-t to-m))
           (let [onc-t (disj onc-t to-m)]
             (println "SECOND TRUE for " to-m "on iter " iter)
             (recur
              (concat signals (map (fn [[dest sign]] [to-m sign dest]) new-signals))
              (assoc machine to-m module)
              nev-t onc-t))
           :else
             (recur
               (concat signals (map (fn [[dest sign]] [to-m sign dest]) new-signals))
               (assoc machine to-m module)
               nev-t onc-t))
           ))
           ))

(defn inverse-machine
  [machine]
  (->>
   machine
   (mapcat (fn[[k v]] (get v "d")))
   set
   (map (fn [n]
          [n (->> machine
               (map (fn [[name info]] (when (some #(= n %) (get info "d")) name)) )
               (keep identity)
                  (into #{} )
             )])
        )
    (into {}))

  )

;; Taken from here : https://rosettacode.org/wiki/Least_common_multiple
(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b, (mod a b))))

(defn lcm
  ([a b]
   (/ (* a b) (gcd a b)))
;;   ([& v]
;;    (reduce lcm v))
  )
;; to calculate the lcm for a variable number of arguments
(defn lcmv [& v] (reduce lcm v))


(defn propagate-fqc-false
  [machine]
  (loop [knwon-fqc [["broadcaster" 1]]
        inverse-m (inverse-machine machine)]
    (when (not (empty? knwon-fqc))
  (let [[[n fq] & knwon-fqc] knwon-fqc 
        module (get machine n)
        ;; type (get module "t")
        dest (get module "d")
        new-inverse-m (reduce (fn [im d] (assoc im d (conj (disj (get im d) n) fq))) inverse-m dest)
        can-count-fqc (filter (fn [[n origines]] (when (reduce #(and %1 %2) (map #(not (string? %)) origines)) n)) new-inverse-m)
        ;; _ (println inverse-m)
        ;; _ (println new-inverse-m)
        _ (println n "-- fq of false =" fq " -->" (apply str (cljstr/join " " dest)))
        [new-known-fqc new-inverse-m]
        (loop [ccfq can-count-fqc new-known-fq [] inv-m new-inverse-m]
          (if (empty? ccfq)
            [new-known-fq inv-m]
            (let [[[n2 fqs] & ccfq] ccfq
                  module2 (get machine n2)
                  type2 (get module2 "t")]
              (println "  " n2 " " type2 " " fqs)
              (case type2
                \% (println "   --> fqc" (* 2 (apply max fqs)))
                \& (println "   --> fqc" (apply lcmv fqs))
                (println "   --> fqc" (apply max fqs)))
              (case type2
                \% (recur ccfq (concat new-known-fq [[n2 (* 2 (apply max fqs))]]) (dissoc inv-m n2))
                \& (recur ccfq (concat new-known-fq [[n2 (apply lcmv fqs)]]) (dissoc inv-m n2))
                (recur ccfq new-known-fq (dissoc inv-m n2))
                ))))
        ]
    ;; (prn knwon-fqc new-known-fqc)
    (recur (concat knwon-fqc new-known-fqc) new-inverse-m))
    
    )))

(defn d20p2
  [input]
  (let [machine (parse-input input)
        all-conj (keys (filter #(= \& (get (second %) "t")) machine))
        machine (reduce init-state-for-conj machine all-conj)
        inv-machine (inverse-machine machine)]
    ;; (propagate-fqc-false machine)
    ;; (draw machine)
    (prn inv-machine)
    (prn (get inv-machine "rx"))
    (prn (get inv-machine "qn"))

    (loop [m machine
           never-true  (set (get inv-machine "qn")) ;;(set (keys machine))
           once-true #{}
           iter 1]
      (let [[m never-true once-true iter] (one-button-push-first-time-true m never-true once-true iter)]
        (if (and (empty? never-true) (empty? once-true))
          iter
          (recur m never-true once-true iter))
        )
    )

    #_(loop [m machine
             found-patterns {}
             to-be-found-patterns  (reduce #(assoc %1 %2 []) {} (get inv-machine "qn"))
             iter 0]
        (let [[n-m n-f-p n-tbf-p n-iter] (one-button-push-with-patterns m found-patterns to-be-found-patterns iter)]
          (when (zero? (mod iter 1000))
            (println "  iter:" n-iter "\n\tfound patterns:" n-f-p "\n\tsearching:" n-tbf-p))
          (if (empty? n-tbf-p)
            [n-iter n-f-p]
            (recur n-m n-f-p n-tbf-p n-iter))))


    #_(loop [machine-in machine
             Npush 1]
        (when (zero? (mod Npush 1000))
          (println "  Npushes: " Npush))
        (let [[machine-in isactivated?] (one-button-push-is-rx? machine-in)]
          (if isactivated?
            Npush
            (recur machine-in (inc Npush))))))
  )

(defn -main
  [& args]
  (println "day20")
  (newline)

;;   (println sample)
;;   (prn (d20p2 sample))
;;   (println "sample2")
;;   (prn (d20p2 sample2))

  (prn (d20p2 (slurp "input/day20.txt")))

  
  )
