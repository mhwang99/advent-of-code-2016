
(defn prs
  [s]
  (if (re-find #"value" s)
    (let [[v b] (mapv #(Integer. %) (re-seq #"\d+" s))]
      [:go b v])
    (let [l (vec (re-seq #"(bot|output) (\d+)" s))
          b (Integer. (nth (first l) 2))
          [bl ol] (if (= (get-in l [1 1]) "bot")
                    (vector (Integer. (get-in l [1 2])) nil)
                    (vector nil (Integer. (get-in l [1 2]))))
          [bh oh] (if (= (get-in l [2 1]) "bot")
                    (vector (Integer. (get-in l [2 2])) nil)
                    (vector nil (Integer. (get-in l [2 2]))))]
      [:gv b bl bh ol oh])))

(defn botc [lgv]
  (reduce (fn [m gv]
            (if (get m (second gv))
              (update m (second gv) conj gv)
              (assoc m (second gv) [gv])))
          {} lgv))

(defn getv
  [m b]
  (let [vs (vec (sort (get m b)))]
    (when (and (= 17 (first vs)) (= 61 (second vs)))
      (println "FOUND " b))
    vs))

(defn go
  [[_ b v] m]
  (let [m (update m b conj v)]
    m))

(defn gv
  [[_ b bl bh ol oh] m o]
  (let [[vl vh] (getv m b)
        m (cond-> m
            true (dissoc b)
            bl (update bl conj vl)
            bh (update bh conj vh))
        o (cond-> o
            ol (update ol conj vl)
            oh (update oh conj vh))]
    [m o]))

(defn movechip
  [[m o mgv] b]
  (if (seq (first (get mgv b)))
    (let [[m o] (gv (first (get mgv b)) m o)
          mgv (->> (vec (rest (get mgv b)))
                   (assoc mgv b))]
      [m o mgv])
    [m o mgv]))

(defn movechips
  [m o mgv]
  (let [lb (filter #(> (count (get m %)) 1) (keys m))]
    (if (empty? lb) [m o mgv]
      (apply movechips (reduce movechip [m o mgv] lb)))))

(defn fp1
  [l]
  (let [lc (map prs l)
        lgo (filter #(= (first %) :go) lc)
        mgv (botc (filter #(= (first %) :gv) lc))]
    (loop [m {} o {}
           lgo lgo mgv mgv]
      (if (empty? lgo) [m o]
      (let [m (go (first lgo) m)
            [m o mgv] (movechips m o mgv)]
        (recur m o (rest lgo) mgv))))))

(defn fp2
  [l]
  (let [[_ m] (fp1 l)]
    (* (first (get m 0))
       (first (get m 1))
       (first (get m 2)))))

(fp1 (str/split (slurp "day10.txt") #"\n+"))
(fp2 (str/split (slurp "day10.txt") #"\n+"))

