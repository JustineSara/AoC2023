(ns t.d19
  (:gen-class)
  (:require
   [clojure.string :as cljstr]
   [clojure.set :as cljset]))

(def sample "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}
")



;; I assume all pieces have all values assigned

;; learned some stuff :
;; [t s] ["a<2006" "qkq"]
;; [_ t-key t-symb t-value] (re-find #"([a-zA-Z]+)(.+?)(\d+)" t)
;; t-value (parse-long t-value)
;; t-symb (resolve (symbol t-symb))  <-- this is the function
    ;; #_(fn [desc]
    ;; (cond
    ;;   (t-symb (get desc t-key) t-value) s)  ))


(defn create-wf-rules
  [wf-list]
  (prn wf-list)
  (let [[wf-l1 wf-else] ((juxt drop-last last) wf-list)]
    (->> wf-l1
         (partition 2)
         (map (fn [[t r]] (let [[_ t-key t-symb t-value] (re-find #"([a-zA-Z]+)(.+?)(\d+)" t)]
                            (str "(" t-symb " (get desc \"" t-key "\") " t-value ") \"" r "\" ")
                            ) ))
         (apply str)
         (#(str "(fn [desc] (cond " % ":else \"" wf-else "\" ))"))
        ;;  (#((prn %) %))
         read-string
         eval
         )))

(defn parse-input
  [input]
  (let [[inst piec] (map cljstr/split-lines (cljstr/split input #"\n\n"))]
    (->>
     inst
     (map (fn [inst]
            (let [[_ name tests] (re-find #"([a-zA-Z]+)\{(.*)\}" inst)]
              [name (create-wf-rules (cljstr/split tests #",|:"))])))
    )
    
    ))





(defn d19
  [input]
  (let [fct1 (create-wf-rules (parse-input input))]
    (prn fct1)
    ;; (println "a<2006" "qkq")
    
    (println "with {a 2010 m 200}" (fct1 {"a" 2010 "m" 200}))
    (println "with {a 2010 m 3000}" (fct1 {"a" 2010 "m" 3000}))
    (println "with {a 200 m 200}" (fct1 {"a" 200 "m" 200}))
    ;; (println (fct1 {"a" 20000}))
    ;; (println (fct1 {"b" 20000}))

    ))

(defn d19p2
  [input]
  (let []
    input))

(defn -main
  [& args]
  (println "day19")

  (println "part1")
  (println sample)
  (prn (d19 sample))

;;   (newline)
;;   (prn (d19 (slurp "input/day19.txt")))


  (newline)
;;   (println "part 2")
;;   (prn (d19p2 sample))
;;   (newline)
;; ;;   (println (d19p2 (slurp "input/day19.txt")))
  )
