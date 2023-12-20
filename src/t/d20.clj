(ns t.d20
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
    [(subs module 1) (first module)]
    ))

(defn init-state-for-conj
  [machine c]
  (->> machine
       (map (fn [[name info]] (when (some #(= c %) (get info "d")) [name false])))
       (keep identity)
       (into {})
       (assoc-in machine [c "s"] )
  ))

(defn parse-input
  [input]
  (->>
    (cljstr/split-lines input)
    (map (fn [l] (cljstr/split l #" -> ")))
    (map (fn [[module destinations]]  [(parse-module module) (cljstr/split destinations #", ")] ))
    (map (fn [[[name type] dest]] [name {"t" type "d" dest "s" (when (= type \%) false)}] ))
    (into {})
   )
)


(defn signal-through-1-module
  " from-m is the origine of the signal
  signal-in is true (high signal) or false (low signal)
  output : [signals-out module]
           with signals-out a list of ['module-name' signal]
           and module the module with updated state"
  [from-m signal-in module]
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
        [(map (fn [d] [d (not (reduce #(and %1 %2) (vals new-state)))]) dest) (assoc module "s" new-state)]
        )
      [[] module]
      )
    ))


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
          (assoc machine to-m module))))
    ))
  ([machine Ntot Nhigh]
   (let [[new-machine addNtot addNhigh] (one-button-push machine)]
        [new-machine (+ addNtot Ntot) (+ addNhigh Nhigh)]))
  )

(defn bruteforce
  [machine-in ntimes]
  (reduce (fn [i _] (apply one-button-push i)) [machine-in 0 0] (range ntimes))

  )

(defn d20p1
  [input]
  (let [machine (parse-input input)
        all-conj (keys (filter #(= \& (get (second %) "t")) machine))
        machine (reduce init-state-for-conj machine all-conj)
        ]
    ;; (signal-through-1-module "button" false (get machine "broadcaster"))
    ;; (signal-through-1-module "broadcaster" false (get machine "a"))
    ;; (one-button-push machine)
    (->> 
     (bruteforce machine 1000)
     ((fn [[_ Ntot Nhigh]] (* Nhigh (- Ntot Nhigh))))
     )
  ))


(defn one-button-push-is-rx?
  ([machine-in]
   (loop [signals [["button" false "broadcaster"]] ;; list of signals to be treated
          Ntotal 0
          Nhigh 0
          machine machine-in]
     (if
       (empty? signals)
       [machine false]
       (let [[[from-m signal to-m] & signals] signals
             [new-signals module] (signal-through-1-module from-m signal (get machine to-m))]
        ;;  (println "  signal:" from-m "--" signal "-->" to-m)
         (if
          (and (= to-m "rx") (not signal))
           [machine true]
         (recur
          (concat signals (map (fn [[dest sign]] [to-m sign dest]) new-signals))
          (inc Ntotal)
          (if signal (inc Nhigh) Nhigh)
          (assoc machine to-m module))))))))

(defn d20p2
  [input]
  (let [machine (parse-input input)
        all-conj (keys (filter #(= \& (get (second %) "t")) machine))
        machine (reduce init-state-for-conj machine all-conj)]
    (loop [machine-in machine
           Npush 1]
      (when (zero? (mod Npush 1000))
        (println "  Npushes: " Npush))
      (let [[machine-in isactivated?] (one-button-push-is-rx? machine-in)]
        (if isactivated?
          Npush
          (recur machine-in (inc Npush)))
        )
      )
  ))

(defn -main
  [& args] 
  (println "day20")
  (newline)
  
  (println "part1")
  (println sample)
  (prn (d20p1 sample))
  (println "sample2")
  (prn (d20p1 sample2))

  (prn (d20p1 (slurp "input/day20.txt")))
  
 (newline)
 (println "part2")
;;  (prn (d20p2 sample))
 (prn (d20p2 (slurp "input/day20.txt")))
  )
