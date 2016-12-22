
(def src (->> (str/split (slurp "day22.txt") #"\n")
              (mapv
                (fn[s]
                  (map #(Integer. %) (drop-last 1 (drop 3 (re-seq #"\d+" s))))))))

(defn viable
  [l [a b]]
  (let [a (nth l a)
        b (nth l b)]
    (if (or (= 0 (nth a 0))
            (> (nth a 0) (nth b 1))) false
      true)))

;part1
(->> (for [a (range 875)
           b (range 875)
           :when (not= a b)]
       [a b])
     (filter #(viable src %))
     count)

(defn redraw
  [l ylen mx]
  (let [[a b] (get l (- mx ylen))
        v (+ a b)]
    (-> (mapv (fn [[a b]]
            (cond
              (= 0 a) \z
              (> (+ a b) (* v 2)) \#
              :else \.)) l)
        (assoc (- mx ylen) \g)
        )))

(defn printl
  [l ylen]
  (let [sl (mapv #(apply str %) (partition ylen l))]
    (println (count sl))
    (doseq [s sl]
      (println s))))

;part2 - way1 : print and count by finger : really fast
(printl (redraw src 25 (* 35 25)) 25)

(defn summary [l]
  [(.indexOf l \z) (.indexOf l \g)])

(defn mov
  [l a b his]
  (let [bb (get l b)]
    (if (= bb \#) nil
      (let [l (-> l
                  (assoc b \z)
                  (assoc a bb))]
        (if (some #{(summary l)} his) nil
          l)))))

(defn getnxt
  [l ylen mx his]
  (let [a (.indexOf l \z)
        g (.indexOf l \g)
        bs (cond-> []
             (>= (- a ylen) 0) (conj (- a ylen))
             (< (+ a ylen) mx) (conj (+ a ylen))
             (> (mod a ylen) 0) (conj (dec a))
             (> (mod (inc a) ylen) 0) (conj (inc a)))]
    (->> bs
         (mapv #(mov l a % his))
         (filterv identity))))

(defn goal?
  [ll]
  (reduce #(if (= \g (get %2 0))
             (reduced true)
             %1) false ll))

(defn proc
  [l ylen]
  (let [mx (count l)
        l (redraw l ylen mx)]
    (loop [his [] ll [l] n 0]
      (println "---" n "(" (count his)")")
      (let [his (into his (mapv summary ll))
            ll (reduce #(into %1 (getnxt %2 ylen mx his)) #{} ll)]
        (cond
          (empty? ll) -1
          (goal? ll) (inc n)
          :else (recur his ll (inc n)))))))

;part2 - way2 : using logic, it take long long time
(proc src 25)

