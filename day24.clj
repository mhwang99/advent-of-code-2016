
(def src (clojure.string/split (slurp "day24.txt") #"\n"))

(defn parse-list
  [l]
  (let [xlen (count (first l))
        l (flatten (map seq l))
        nos (reduce #(let [c (char (+ 48 %2))
                           idx (.indexOf l c)]
                       (if (>= idx 0) (conj %1 c)
                         (reduced %1))) [] (range 1 10))]
    [xlen nos l]))

(defn permutations [s]
  (lazy-seq
   (if (seq (rest s))
     (apply concat (for [x s]
                     (map #(cons x %) (permutations (remove #{x} s)))))
     [s])))

(defn get-nxtpath
  [p l xlen his]
  (let [bs [(dec p) (inc p) (conj (- p xlen)) (conj (+ p xlen))]]
    (->> bs
        (filter (complement his))
        (filter #(not (#{\#} (nth l %)))))))

(defn get-minmove
  [l xlen init target m]
  (if-let [mn (get-in m [init target])] [m mn]
    (let [mn (loop [his #{init} ps [init] n 0]
               (let [[his ps] (reduce (fn [[his ps] p]
                                        (let [cps (get-nxtpath p l xlen his)
                                              ps (into ps cps)
                                              his (into his ps)]
                                          [his ps]))
                                      [his []] ps)]
                 (cond
                   (empty? ps) -1
                   (some #{target} ps) (inc n)
                   :else (recur his ps (inc n)))))
          m (-> m
                (assoc-in [init target] mn)
                (assoc-in [target init] mn))]
      [m mn])))

(defn proc-path
  [l xlen path m]
  (let [path (map #(.indexOf l %) path)]
    (loop [l l init (.indexOf l \0) path path cnt 0 m m]
      (if (empty? path) [m cnt]
        (let [target (first path)
              [m n] (get-minmove l xlen init target m)]
          (recur l target (rest path) (+ cnt n) m))))))

(defn part1
  [l]
  (let [[xlen nos l] (parse-list l)
        paths (permutations nos)
        [n m] (reduce (fn [[ret m] path]
                        (let [[m n] (proc-path l xlen path m)]
                          (if (< n ret) [n m] [ret m])))
                      [(* 9 (count l)) {}] paths)]
    n))

(defn part2
  [l]
  (let [[xlen nos l] (parse-list l)
        paths (permutations nos)
        [n m] (reduce (fn [[ret m] path]
                        (let [path (conj (vec path) \0)
                              [m n] (proc-path l xlen path m)]
                          (if (< n ret) [n m] [ret m])))
                      [(* 9 (count l)) {}] paths)]
    n))

;; part1
(part1 src)

;; part2
(part2 src)
