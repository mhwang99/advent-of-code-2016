
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


(defn proc
  [cmds rg]
  (let [cnt (count cmds)
        getv (fn [rg a] (if (keyword? a) (a rg) a))]
    (loop [rg rg n 0]
      (if (> n (dec cnt)) rg
        (let [[cmd a b] (nth cmds n)]
          (case cmd
              :inc (recur (update rg a inc) (inc n))
              :dec (recur (update rg a dec) (inc n))
              :cpy (recur (assoc rg b (getv rg a)) (inc n))
              :jnz (if (not= 0 (getv rg a))
                     (recur rg (+ n b))
                     (recur rg (inc n)))))))))

(proc (parse (clojure.string/split (slurp "day12.txt") #"\n")) {:a 0 :b 0 :c 0 :d 0})
(proc (parse (clojure.string/split (slurp "day12.txt") #"\n")) {:a 0 :b 0 :c 1 :d 0})
