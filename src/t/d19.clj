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
;;   (prn wf-list)
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
    [(->>
       inst  ;; one inst : px{a<2006:qkq,m>2090:A,rfg}
       (map (fn [inst]
              (let [[_ name tests] (re-find #"([a-zA-Z]+)\{(.*)\}" inst)]
                [name (create-wf-rules (cljstr/split tests #",|:"))])))
       (into {})
    )
     (->> piec  ;; one piec : {x=787,m=2655,a=1222,s=2876}
          (map #(cljstr/replace % "=" "\" "))
          (map #(cljstr/replace % "," " \""))
          (map #(cljstr/replace % "{" "{\""))
          (map #(eval (read-string %)))
        ;; (cljstr/replace "\{" "\{\"")
          )
    ]
    
    ))


(defn go-tgh-wf
  [piece wkfw]
  (loop [nxt-wf "in"]
    ;; (prn nxt-wf)
    (cond
      (= nxt-wf "A") piece
      (= nxt-wf "R") nil
      :else
      (recur ((get wkfw nxt-wf) piece))
    )
  ))



(defn d19
  [input]
  (let [[workflows pieces] (parse-input input)]
    (->>
     pieces
     (map #(go-tgh-wf % workflows))
     (keep identity)
     (mapcat vals)
     (apply +)
    )
    ))



(defn parse-input-2
  [input]
  (let [[inst _] (map cljstr/split-lines (cljstr/split input #"\n\n"))]
    (->>
      inst  ;; one inst : px{a<2006:qkq,m>2090:A,rfg}
      (map (fn [inst]
             (let [[_ name tests] (re-find #"([a-zA-Z]+)\{(.*)\}" inst)]
               [name 
                (let [[ts-pair ts-rest] ((juxt drop-last last) (cljstr/split tests #",|:"))]
                 (concat 
                   (->> 
                     ts-pair
                     (partition 2)
                     (map (fn [[t r]] (let [[_ t-key t-symb t-value] (re-find #"([a-zA-Z]+)(.+?)(\d+)" t)]
                                        {"k" t-key "s" t-symb "v" (parse-long t-value) "next" r}
                                        ))))
                  
                   [{"next" ts-rest}]
                 )
                )
                ])))
      (into {}))))

(defn go-tg-1wk-2
  [piece wk]
  (loop [p piece
         solution []
         step (first wk)
         steps (rest wk)]
   (if 
     (empty? steps)
     (conj solution [(get step "next") p])
     (let [key (get step "k")
           val (get step "v")
           [k-min k-max] (get p key)
           sym (get step "s")
           n (get step "next")]
       (cond 
         (= sym "<")
         (cond 
           (< k-max val) (conj solution [n p]) ;; everything checks the condition and goes to next
           (< val k-min) (recur p solution (first steps) (rest steps)) ;; nothing chekcs the condition and all goes to next tests (recur)
           :else ;; what is <val goes to next , what is >= val goes to more tests
           (recur (assoc p key [val k-max]) (conj solution [n (assoc p key [k-min (dec val)])]) (first steps) (next steps))
           )
         (= sym ">")
         (cond
           (> k-min val) (conj solution [n p])
           (> val k-max) (recur p solution (first steps) (rest steps))
           :else
           (recur (assoc p key [k-min val]) (conj solution [n (assoc p key [(inc val) k-max])]) (first steps) (next steps))
           )
         )
       )  
       )
    
   )
  
  )

(defn through-wf-2
  [start-list-pos wkfws]
  ;; pos = position : is ["next-workflow" {range of values: "a" [min-a max-a] ...}]
  (loop [list-pos start-list-pos
         Accepted []]
    (if 
      (empty? list-pos)
      Accepted
      (let [[this-pos & list-pos] list-pos
            new-pos (go-tg-1wk-2 (second this-pos) (get wkfws (first this-pos)))
            new-accepted (filter #(= (first %) "A") new-pos)
            pos-go-on (filter #(and (not= (first %) "A") (not= (first %) "R")) new-pos)]
        (recur (concat pos-go-on list-pos) (concat Accepted new-accepted))
        )  
        )
    
    )
  )

(defn d19p2
  [input]
  (let [wkfws (parse-input-2 input)
        start ["in" {"x" [1 4000] "m" [1 4000] "a" [1 4000] "s" [1 4000]}]]
    ;; (println start)
    ;; (println (get wkfw (first start)))
    ;; (go-tg-1wk-2 (second start) (get wkfw (first start)))
    (->> 
      (through-wf-2 [start] wkfws)
      (map second)
      (map vals)
      (map (fn [lv] (apply * (map (fn [[minv maxv]] (- (inc maxv) minv)) lv))))
      (apply +)
     )
    ))

(defn -main
  [& args]
  (println "day19")
  (println sample)
  
;;   (println "part1")
;;   (prn (d19 sample))
;;   (newline)
;;   (prn (d19 (slurp "input/day19.txt")))

;;   (newline)

  (println "part 2")
  (println "for sample expect :")
  (prn 167409079868000)
  (prn (d19p2 sample))
  (newline)
  (println (d19p2 (slurp "input/day19.txt")))
  )
