
(defn parse
  [l]
  (let [v (fn [v]
            (try (Integer. v)
                 (catch Exception e (keyword v))))]
    (reduce (fn [cmds s]
              (let [[cmd a b] (clojure.string/split s #"\s+")]
                (conj cmds [(keyword cmd) (v a) (v b)])))
            [] l)))

(defn getv
  [rg a]
  (if (keyword? a) (a rg) a))

(defn updatev
  [rg a f]
  (if (keyword? a) (update rg a f) rg))

(defn assocv
  [rg a v]
  (if (keyword? a) (assoc rg a v) rg))

(defn tglcmd
  [cmds i]
  (if-let [[cmd a b] (get cmds i)]
    (assoc cmds i [(case cmd
                     :inc :dec
                     :dec :inc
                     :tgl :inc
                     :cpy :jnz
                     :jnz :cpy) a b])
    cmds))

(defn proc
  [cmds rg]
  (let [cnt (count cmds)
        getv (fn [rg a] (if (keyword? a) (a rg) a))]
    (loop [cmds cmds rg rg n 0 bv (:b rg)]
      (let [n (if (< n 0) 0 n)
            bv (cond
                 (not= (:b rg) bv) (do (println "========" rg) (:b rg))
                 :else bv)]
        (if (> n (dec cnt)) rg
          (let [[cmd a b] (nth cmds n)]
            (case cmd
              :inc (recur cmds (updatev rg a inc) (inc n) bv)
              :dec (recur cmds (updatev rg a dec) (inc n) bv)
              :cpy (recur cmds (assocv rg b (getv rg a)) (inc n) bv)
              :jnz (if (not= 0 (getv rg a))
                     (recur cmds rg (+ n (getv rg b)) bv)
                     (recur cmds rg (inc n) bv))
              :tgl (recur (tglcmd cmds (+ (getv rg a) n)) rg (inc n) bv)
              )))))))

;; part1
(proc (parse (clojure.string/split (slurp "day23.txt") #"\n")) {:a 7 :b 0 :c 0 :d 0})

;; part2
;; (proc (parse (clojure.string/split (slurp "day23.txt") #"\n")) {:a 6 :b 0 :c 0 :d 0})
;; (proc (parse (clojure.string/split (slurp "day23.txt") #"\n")) {:a 7 :b 0 :c 0 :d 0})
;; (proc (parse (clojure.string/split (slurp "day23.txt") #"\n")) {:a 8 :b 0 :c 0 :d 0})
;; ==> 7440 + 12 * 11 * 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2
(+ 7440 (apply * (range 1 13)))
