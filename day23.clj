
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
    (loop [cmds cmds rg rg n 0]
      (let [n (if (< n 0) 0 n)]
        (if (> n (dec cnt)) rg
          (let [[cmd a b] (nth cmds n)]
            (case cmd
              :inc (recur cmds (updatev rg a inc) (inc n))
              :dec (recur cmds (updatev rg a dec) (inc n))
              :cpy (recur cmds (assocv rg b (getv rg a)) (inc n))
              :jnz (if (not= 0 (getv rg a))
                     (recur cmds rg (+ n (getv rg b)))
                     (recur cmds rg (inc n)))
              :tgl (recur (tglcmd cmds (+ (getv rg a) n)) rg (inc n))
              )))))))

(defn part1
  [src n]
  (let [cmds (parse (clojure.string/split src #"\n"))
        rg {:a n :b 0 :c 0 :d 0}]
    (:a (proc cmds rg))))

(defn part2
  [src n]
  (let [r6 (part1 src 6) ;; x + a = r6
        r7 (part1 src 7) ;; 7x + a = r7
        x (/ (- r7 r6) 6) ;; 6x = r7 - r6 , x = (r7 - r6) / 6
        conv (- r6 x)
        ]
    (+ conv (apply * x (range 7 (inc n)))))) ; x * 7 * 8 * ... n + a

;; part1
(part1 (slurp "day23.txt") 7)

;; part2
(part2 (slurp "day23.txt") 12)
